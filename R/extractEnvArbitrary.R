##' @title Extract and append remote sensing environmental data to detection data
##'
##' @description Accesses and download environmental data from the IMOS THREDDS 
##' server and append variables to detection data based on date of detection
##'
##' @param df detection data source in data frame with at the minimum a X, Y and
##' date time field
##' @param X name of column with X coordinate or longitude (EPSG 4326)
##' @param Y name of column with Y coordinate or latitude (EPSG 4326)
##' @param datetime name of column with date time stamp (Coordinated Universal 
##' Time; UTC)
##' @param env_request ...
##' @param request_type ...
##' @param folder_name name of folder within 'imos.cache' where downloaded rasters 
##' should be saved. default NULL produces automatic folder names based on study extent
##' @param verbose should function provide details of what operation is being conducted. 
##' Set to `FALSE` to keep it quiet
##' @param cache_layers should the extracted environmental data be cached within
##' the working directory? if FALSE stored in temporary folder and discarded 
##' after environmental extraction
##' @param crop_layers should the extracted environmental data be cropped to 
##' within the study site
##' @param full_timeperiod should environmental variables extracted for each day
##' across full monitoring period, time and memory consuming for long projects
##' @param fill_gaps should the function use a spatial buffer to estimate 
##' environmental variables for detections where there is missing data. Default 
##' is `FALSE` to save computational time.
##' @param buffer radius of buffer (in m) around each detection from which 
##' environmental variables should be extracted from. A median value of pixels 
##' that fall within the buffer will be used if `fill_gaps = TRUE`. If `NULL` a 
##' buffer will be chosen based on the resolution of environmental layer. A 
##' numeric value (in m) can be used here to customise buffer radius.
##' @param output_format File type for cached environmental layers. See 
##' \code{\link[raster]{writeFormats}}. The default format is 'raster'.
##' @param .parallel should the function be run in parallel 
##' @param .ncores number of cores to use if set to parallel. If none provided, 
##' uses \code{\link[parallel]{detectCores}} to determine number.
##'
##' @details The `extractEnv` function allows the user to access, download and 
##' append a range of environmental variables to each detection within a telemetry
##' data set. We advocate for users to first undertake a quality control step using
##' the \code{\link{runQC}} function before further analysis, however the 
##' functionality to append environmental data will work on any dataset that has
##' at the minimum spatial coordinates (i.e., latitude, longitude; in EPSG 4326)
##' and a timestamp (in UTC) for each detection event. Quality controlled 
##' environmental variables housed in the IMOS Thredds server will be extracted 
##' for each specific coordinate at the specific timestamp where available. A 
##' summary table of the full range of environmental variables currently 
##' available can be accessed using the \code{\link{imos_variables}} function.
##' 
##'
##' @return a dataframe with the environmental variable appended as an extra 
##' column based on date of each detection
##'
##' @examples
##' ## Input example detection dataset that have run through the quality control
##' ##   workflow (see 'runQC' function)
##' 
##' library(tidyverse)
##' data("TownsvilleReefQC")
##' 
##' ## simplify & subset data for speed
##' qc_data <- 
##'   TownsvilleReefQC %>% 
##'   unnest(cols = c(QC)) %>% 
##'   ungroup() %>% 
##'   filter(Detection_QC %in% c(1,2)) %>%
##'   filter(filename == unique(filename)[1]) %>%
##'   slice(1:20)
##' 
##' ## Extract daily interpolated sea surface temperature
##' ## cache_layers & fill_gaps args set to FALSE for speed
##' data_with_sst <- 
##'   extractEnv(df = qc_data,
##'               X = "receiver_deployment_longitude", 
##'               Y = "receiver_deployment_latitude", 
##'               datetime = "detection_datetime", 
##'               env_var = "rs_sst_interpolated",
##'               cache_layers = FALSE,
##'               crop_layers = TRUE,
##'               full_timeperiod = FALSE,
##'               fill_gaps = FALSE,
##'               folder_name = "test",
##'               .parallel = FALSE)
##'
##' @importFrom dplyr '%>%' mutate distinct pull left_join select
##' @importFrom raster raster extent
##' @importFrom lubridate date 
##' @importFrom progressr with_progress
##'
##' @export
##'

extractEnvArbitrary <-
  function(df,
           X = "longitude",
           Y = "latitude",
           datetime = "datecollected",
           env_request = NULL,
           request_type = 'single',
           folder_name = NULL,
           verbose = TRUE,
           cache_layers = TRUE,
           crop_layers = TRUE,
           full_timeperiod = FALSE,
           fill_gaps = FALSE,
           buffer = NULL,
           output_format = "raster",
           .parallel = TRUE,
           .ncores = NULL) {
    
  
  ## Initial checks of parameters
  if(!X %in% colnames(df)){stop("Cannot find X coordinate in dataset, provide column name where variable can be found")}
  if(!Y %in% colnames(df)){stop("Cannot find Y coordinate in dataset, provide column name where variable can be found")}
  
  #Temporary solution (I hope) while I figure out how best to rebuild parts of the code now that the URL generation part is user-dictated,
  #so as to facilitate arbitrary data sources.
  if(request_type == 'bathy') {
    env_var <- 'bathy'
  }
  else {
    env_var <- env_request$layer[1]
  }
  
  ## Turn off un-needed parallelising or gap filling if extracting 'bathy', 'dist_to_land'
  if(env_var %in% c("bathy", "dist_to_land")){
    .parallel = FALSE
    fill_gaps = FALSE}
  
  if(env_var %in% "rs_current"){.parallel = FALSE}
  
  ## define spatial extent and extend by 40%
  #I'm making this as both a vector and an extent so as to maintain functionality while I work on this
  #The vector is necessary for my code. But the original code uses the extent, and we'll probably still
  #need it. - BD
  spatial_extent <- c(minLon = min(df[[X]]), maxLon = max(df[[X]]), minLat = min(df[[Y]]), maxLat = max(df[[Y]]))
  study_extent <- extent(spatial_extent) * 1.4
  
  ## define unique positions (for quicker environmental variable extraction)
  unique_positions <-
    ungroup(df) %>% 
    mutate(date = as.Date(!!as.name(datetime))) %>%
    distinct(!!as.name(X), !!as.name(Y), date) %>% 
    dplyr::select(!!as.name(X), !!as.name(Y), date)
  
  # Pull environmental netcdf from THREDDS server
  if(verbose){
    message("Accessing and downloading environmental variable: ", env_var)
  }
  
  #These were changed from pull_env to pull_env_arbitrary while I was working on this -- BD
  if(.parallel){
    with_progress(
      try(
        suppressWarnings(
          env_stack <- pull_env_arbitrary(
            urls = env_request,
            request_type = request_type,
            study_extent = study_extent,
            var_name = env_var,
            .cache = cache_layers,
            folder_name = folder_name,
            .crop = crop_layers,
            .output_format = output_format,
            verbose = verbose,
            .parallel = .parallel,
            .ncores = .ncores
          )), 
        silent = FALSE), cleanup = FALSE)
  } else {
    try(
      suppressWarnings(
        env_stack <- pull_env_arbitrary(
          urls = env_request,
          study_extent = study_extent,
          var_name = env_var,
          .cache = cache_layers,
          folder_name = folder_name,
          .crop = crop_layers,
          .output_format = output_format,
          verbose = verbose,
          .parallel = .parallel,
          .ncores = .ncores
        )),
      silent = FALSE) 
  }
  
  if(cache_layers & verbose){
    message("\nDownloaded layers are cached in the `imos.cache` folder in your working directory")
  }
  
  #View(env_stack)
  #image(env_stack)
  
  ## Extract environmental variable from env_stack
  if(verbose){
    message("Extracting and appending environmental data")
  }
  env_data <- .extract_var(unique_positions, env_stack, env_var, .fill_gaps = fill_gaps, .buffer = buffer, verbose = verbose)
  
  #view(env_data)
  
  ## Combine environmental data with input detection data
  output <- 
    df %>% 
    mutate(date = as.Date(!!as.name(datetime))) %>%
    left_join(env_data, by = c(X, Y, "date"))
  
  
  ## Calculate additional variables for current data (current direction and velocity)
  if(env_var %in% "rs_current"){
    output <-
      output %>% 
      mutate(rs_current_velocity = sqrt(rs_vcur^2 + rs_ucur^2),
             rs_current_bearing = atan2(rs_ucur,rs_vcur)*(180/pi))
    
    ## Adjust bearing to 0 - 360 degrees clockwise
    output <- 
      output %>% 
      mutate(rs_current_bearing = 
               case_when(rs_current_bearing < 0 ~ rs_current_bearing + 360, 
                         TRUE ~ rs_current_bearing))
  }
  
  output$date <- NULL
  
  return(output)
}
