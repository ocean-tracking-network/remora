##' @title run IMOS-ATF acoustic detections quality control process
##'
##' @description conduct quality control (QC) on IMOS-ATF acoustic detections data
##'
##' @param x a 4-element list of paths to detections, receiver and transmitter
##' deployment metadata, and animal measurements data files. These data must be
##' downloaded from the IMOS-ATF Web App (URL), or have exactly the same
##' structure and variable names as the Web App data.
##' @param lat.check (logical; default TRUE) test for receiver_deployment_latitudes
##' in N hemisphere and correct to S hemisphere. Set to FALSE if QC'ing N hemisphere data
##' @param data_format specify data format as a quoted character string ("imos" or "otn")
##' @param tests_vector a vector of possible QC tests as quoted character strings.
##' All eight tests are: "FDA_QC", "Velocity_QC", "Distance_QC", "DetectionDistribution_QC", 
##' "DistanceRelease_QC", "ReleaseDate_QC", "ReleaseLocation_QC", and "Detection_QC".
##' The default is to run all tests.
##' @param shapefile Optional user-specified shapefile defining species range(s).
##' @param col_spec Optional specification of input detection data column types 
##' (see `read_csv` for details on col_types).
##' @param fda_type specify whether to use the time difference (Hoenner et al. 2018)
##' or the Pincock (Pincock 2012) algorthim for the False Detections test 
##' (`time_diff` or `pincock`).
##' @param .parallel logical; run QC tests in parallel across multiple processors
##' (default is FALSE)
##' @param .ncores integer; number of cores to run in parallel. If NULL and
##' \code{parallel = TRUE} then process will run across all available cores,
##' otherwise run across user-specified cores
##' @param .progress logical; display QC progress (default is TRUE).

##'
##' @details The QC process merges data from the supplied files downloaded via
##' the IMOS-ATF Web App (URL): `IMOS_detections.csv`;
##' `IMOS_receiver_deployment_metadata.csv`;
##' `IMOS_transmitter_deployment_metadata.csv`;
##' and `IMOS_animal_measurements.csv`. Eight quality control tests are
##' performed on the detections, as outlined in Hoenner et al. (2018), and
##' QC flags are appended to the merged data for each of these 8 tests.
##'
##' The QC flags are values ranging between 1 and 4, representing `valid`,
##' `likely valid`, `likely invalid`, and `invalid` detections, respectively.
##' The user can then employ these flags, in various combinations, to filter the
##' merged data (see \code{examples} & \code{vignette}).
##'
##' Utility functions are provided to subset the merged data in various ways
##' from the nested tibble and to visualise the QC results (see \code{examples} 
##' & \code{vignette}).
##'
##' A QC log is written to `QC_logfile.txt` in the working directory. The 
##' logfile documents potential data issues discovered during the QC process: 
##' e.g., `transmitter_deployment_id`'s present in the detections
##' file but not in the transmitter metadata file (if supplied);
##' `receiver_deployment_id`'s present in the detections file but not in the
##' receiver metadata file (if supplied); NA's present in transmitter deployment
##'  locations. Generally, these issues can not be corrected automatically and
##' require the user to investigate the cause and take appropriate steps to
##' correct the data.
##'
##' @return the QC output is returned to the parent frame as a nested tibble
##' with class `remora_QC`
##'
##' @references Hoenner, X et al (2018) Australia’s continental-scale acoustic
##' tracking database and its automated quality control process. Scientific Data
##' 5, 170206. https://doi.org/10.1038/sdata.2017.206
##' 
##' Pincock, DG (2012) False detections: what they are and how to remove them 
##' from detection data. Amirix Document DOC-004691 Version 03. Amirix Systems Inc., 
##' Halifax, Nova Scotia. 
##'
##' @examples
##' ## specify files to QC - use supplied example .csv data
##' files <- list(det = system.file(file.path("test_data","IMOS_detections.csv"),
##'                     package = "remora"),
##'               rmeta = system.file(file.path("test_data",
##'               "IMOS_receiver_deployment_metadata.csv"),
##'                     package = "remora"),
##'               tmeta = system.file(file.path("test_data",
##'               "IMOS_transmitter_deployment_metadata.csv"),
##'                     package = "remora"),
##'               meas = system.file(file.path("test_data",
##'               "IMOS_animal_measurements.csv"),
##'                     package = "remora"))
##' qc.out <- runQC(files)
##' plotQC(qc.out, path = ".") # saves .html file to working directory
##'
##' ## get detections with QC flags
##' d.qc <- grabQC(qc.out, what = "dQC")
##' 
##' ## clean up
##' system("rm QC_logfile.txt *_QCmap.html")
##'
##' @importFrom stringr str_split
##' @importFrom readr read_csv cols col_character col_double col_integer col_datetime
##' @importFrom lubridate ymd_hms dmy_hm
##' @importFrom dplyr %>% rename mutate nest_by bind_rows
##' @importFrom parallel detectCores
##' @importFrom future plan
##' @importFrom furrr future_map furrr_options
##' @importFrom utils flush.console
##'
##' @export

runQC <- function(x,
                  lat.check = TRUE,
                  data_format = "imos", 
                  tests_vector = c("FDA_QC",
                                     "Velocity_QC",
                                     "Distance_QC",
                                     "DetectionDistribution_QC",
                                     "DistanceRelease_QC",
                                     "ReleaseDate_QC",
                                     "ReleaseLocation_QC",
                                     "Detection_QC"), 
                  shapefile = NULL, 
                  col_spec = NULL, 
                  fda_type = "time_diff", #Added by Bruce Delo for pass-through to QC, then to false detections. Lets user decide whether to use remora's time diff method or pincock method.
                  rollup = FALSE, #Added by Bruce Delo, invokes Surimi's rollup function to return QC columns attached to detection extract.
                  world_raster = NULL, #Added by Bruce Delo, pass-through to QC for use as something other than Australia's raster for shortest-distance calculation.
                   .parallel = FALSE,
                   .ncores = detectCores() - 2,
                   .progress = TRUE,
                  ...) {

  ## check if n_cores <= detectCores else return warning
  if(.ncores > detectCores())
    warning("process to be run across more cores than available, this may not be efficient",
            call. = FALSE, immediate. = TRUE)
  
  ## create logfile object to record QC warnings
  ## write logfile to working directory
  logfile <- "QC_logfile.txt"
  write("", file = logfile)

  message("Reading data...")
  ## IDJ: add conditional to use arbitrary or canonical get_data fn
  all_data <- switch(data_format,
                     otn = {
                       get_data_arbitrary(
                         det = x$det,
                         rmeta = x$rmeta,
                         tmeta = x$tmeta,
                         meas = x$meas,
                         logfile = logfile,
                         data_format = "otn",
                         col_spec = col_spec
                       )
                     },
                     imos = {
                       get_data(
                         det = x$det,
                         rmeta = x$rmeta,
                         tmeta = x$tmeta,
                         meas = x$meas,
                         logfile = logfile
                       )
                     })
  
  #Set up a raster for the world (temporary while I test the QC functions that require shapefiles to work)
  #world_raster <- readOGR(dsn = 
                           # file.path("/Users/bruce/Downloads/Land_Masses_and_Ocean_Islands/Land_Masses_and_Ocean_Islands.shp"),
                         # verbose = F)
  
  ## Apply QC tests on detections
  if(.parallel) {
    message("Starting parallel QC...")
    plan("multisession", workers = .ncores)

    QC_result <- future_map(all_data, 
                            try(qc, silent = TRUE), 
                            Lcheck = lat.check, 
                            logfile, 
                            .progress = .progress, 
                            .options = furrr_options(seed = TRUE))

    plan("sequential")
  } else {
    message("Starting sequential QC...")
    QC_result <- lapply(1:length(all_data), function(i) {
      if(.progress) {
        cat("\r", "file: ", all_data[[i]]$filename[1], ", ", i, " of ", length(all_data), "    ", sep = "")
        flush.console()
      }
      
      if(data_format == "otn") {
        message("Starting OTN QC")
        try(qc(all_data[[i]],
               Lcheck = FALSE,
               logfile,
               tests_vector,
               data_format = "otn",
               shapefile = shapefile,
               fda_type = fda_type,
               world_raster = world_raster,
               ...), 
            silent = FALSE)
        
      } else if (data_format == "imos") {
        suppressMessages(try(qc(all_data[[i]],
                                Lcheck = lat.check,
                                logfile,
                                tests_vector,
                                data_format = "imos"),
                             silent = TRUE)
        )
      }
      
     })

    cat("\n")
  }

  ## warn of any QC failures
  fails <- sapply(QC_result, function(x) inherits(x, "try-error"))
  nfail <- sum(fails)
  ## write `try-error` to logfile
  if(nfail > 0) {
    warning(paste(nfail, "tag detection file(s) could not be QC'd"),
            call. = FALSE, immediate. = TRUE)
    xfail <- all_data[fails]
    lapply(1:length(xfail), function(i) {
      write(paste0(xfail[[i]]$filename[1], ":  QC error: ", QC_result[[i]]),
        file = logfile,
        append = TRUE
      )
    })
  }

  ## notify if any entries in QC logfile
  if(file.size(logfile) > 1) {
    message("\n Please see ", logfile, " for potential data, metadata issues and/or QC error messages\n")
  }
  ## IDJ: modified so fn returns QC results for any tags that did not fail the QC
  tmp <- bind_rows(QC_result[!fails])
  out <- nest_by(tmp, filename, .key = "QC")
  class(out) <- append("remora_QC", class(out))

  ## warn if any NA's in detection_datetime (caused by impossible dates,
  ##  eg. 30 Feb or 31 Feb)
  NA_datetime <- sapply(out$QC, function(x) sum(is.na(x$detection_datetime)) > 0)
  if(sum(NA_datetime) > 0) {
    warning(paste("NA's found in `detection_datetime` for:",
                  out$filename[NA_datetime], "\n"),
            call. = FALSE,
            immediate. = TRUE)
  }

  if(rollup == TRUE) {
    #Start by writing out the QC
    #Don't alter the path, so then we can assume it based on where we're at. 
    writeQC(out, aggregate = TRUE)
    rolled_output <- surimi::rollup(x$det, "aggregatedQC.csv")
    write.csv(rolled_output, file="rolled_output.csv")
  }
  
  return(out)
}
