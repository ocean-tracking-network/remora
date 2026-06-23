##' @title test the validity of individual detections for tags detected more than once
##'
##' @description Subjects tag detections to 8 quality control tests and appends results to each detection record
##'
##' @param x a list of un-QC'd data for each tag deployment
##' @param Lcheck (logical; default TRUE) test for receiver_deployment_latitudes
##' in N hemisphere at correct to S hemisphere. Set to FALSE for QC on N hemisphere data
##' @param logfile path to logfile; default is the working directory
##' @param tests_vector ...
##' @param data_format currently, "imos" (default) or "otn"
##' @param shapefile A shapefile for species home ranges. Can be left null but some tests may not run. 
##'
##' @details ...
##'
##' @return temporal_outcome is a list with each element corresponding to a QC'd tag detection file
##'
##' @importFrom sp 'coordinates<-' 'proj4string<-' 'proj4string' over SpatialPoints
##' @importFrom geosphere distGeo
##' @importFrom glatos make_transition2
##' @importFrom dplyr %>% bind_cols
##' @importFrom sf st_as_sf st_distance st_crs st_intersects st_coordinates
##' @import stringr str_to_camel
##'
##' @keywords internal
##'


qc <- function(x, Lcheck = TRUE, logfile, tests_vector = c("FDA_QC",
                                                           "Velocity_QC",
                                                           "Distance_QC",
                                                           "DetectionDistribution_QC",
                                                           "DistanceRelease_QC",
                                                           "ReleaseDate_QC",
                                                           "ReleaseLocation_QC",
                                                           "Detection_QC"),
               data_format = "imos",
               shapefile = NULL,
               fda_type = "time-diff",
               dist_threshold = 500,
               world_raster = NULL,
               qcArguments = NULL, #Optional pass-through from runQC, contains arguments for species-specific QC parameters. 
               ...) {
  if(!is.data.frame(x)) stop("x must be a data.frame")
  ## Configure output processed data file
  temporal_outcome <- data.frame(matrix(ncol = length(tests_vector), nrow = nrow(x)))
  colnames(temporal_outcome) <- tests_vector
  
  #If our dataframe is only one entry long, quit.
  if(nrow(x) == 1) {
    message("Dataframe must have more than one row for QC.")
    return(bind_cols(x, temporal_outcome))
  }
  
  #Start by removing any rows that have NAs in the datetime, lat, or long columns. I'd like to return to this function and make something
  #a little more comprehensive but for now I've just sliced out the code and hived it off into its own function for cleanliness' sake.
  #message("Removing NAs")
  x <- qc_remove_nas(x)
  #message("NAs removed.")
  
  #I've commented out this check for now, I think we're not going to want this what with our intended global scope. 
  ## IDJ - uncommented as latitude check is useful for IMOS data. I've added to the conditional so this only gets 
  ##  implemented if the data_format = "imos"
  ## check for & correct any lat's incorrectly in N hemisphere
  if(any(x$latitude > 0) & Lcheck & data_format == "imos") {
   ## how many incorrect records
   n <- sum(x$latitude > 0)
   ## write to logfile
   write(paste0(x$filename[1],
               ":  ", n, " receiver_deployment_latitude(s) incorrectly entered in N hemisphere; corrected in QC output"),
         file = logfile,
         append = TRUE)

     x <- x %>% mutate(latitude = ifelse(latitude > 0, -1 * latitude, latitude))
  }
  
  #Removed sections flagged as redundant. - BD 30/06/2022
  
  #Let the user know what file (tag) we're operating on. 
  message(x$filename[1])
  write(paste0(x$filename[1],":  ", " Grabbing species shapefile."),
        file = logfile,
        append = TRUE)
  
  #This preserves the original IMOS functionality, which uses the ALA (Atlas of Living Australia) to get a shapefile. 
  if(data_format == "imos") {
    spe <- unique(x$species_scientific_name)
    CAAB_species_id <- unique(x$CAAB_species_id)
  
    ## Find corresponding ALA shapefile based on species name
    shp_b <- NULL
    if (!is.na(spe) & !is.na(CAAB_species_id))
      shp_b <- try(get_expert_distribution_shp(CAAB_species_id, spe))
  
    ## if no shape file or spe or CAAB_species_id is missing then append to logfile & continue
    if(is.null(shp_b)) {
      ## write to logfile
      write(paste0(x$filename[1],
                  ": shapefile not available for ", spe, "; Dectection distribution not tested"),
            file = logfile,
            append = TRUE)
    } else if(inherits(shp_b, "try-error")) {
      ## write to logfile
      write(paste0(x$filename[1],
                   ": shapefile could not be downloaded for ", spe, "; Dectection distribution not tested"),
            file = logfile,
            append = TRUE)
      shp_b <- NULL
    }
  } else if (data_format == "otn") {
    arguments <- NULL
    #Meanwhile, this is our version, which uses locally cached versions.
    #We're also going to search the qcParameters we've been given here, since they both rely on a camelcased species name, so now's as good a time as any. 
    
    #Get the species name we're working with.
    species <- unique(x$species_scientific_name)
    #Camel-case it so that it matches what's in the list of polygons returned by createAndFetchPolygons/getPolygonsFromDetectionExtract...
    species <- str_to_camel(species)
    
    #Start by looking for and processing the shapefile. 
    if(is.null(shapefile)) {
      message("WARNING: No shapefile supplied. Some tests may not run.")
      shp_b <- NULL
    }
    else if(is.list(shapefile)) {
      #And now get the polygon from the array. 
      message(paste0("Polygon found for ", species))
      shp_b <- shapefile[[species]]$geometry
    }
    else {
      shp_b <- shapefile  
    }
    
    if(!is.null(qcArguments))
    {
      if(species %in% qcArguments) {
        arguments <- qcArguments[species]
      }
    }
  }
  write(paste0(x$filename[1],
               ":  "," Shapefile Grab done."),
        file = logfile,
        append = TRUE)
  
  ## Converts unique sets of lat/lon detection coordinates and release lat/lon 
  ##  coordinates to SpatialPoints to test subsequently whether or not detections 
  ##  are in distribution range
  if (!is.null(shp_b)) {
    #message("shapefile not null, starting lat/lon conversion to SpatialPoints")

    ll <- unique(data.frame(x$longitude, x$latitude))
    
    ll <- SpatialPoints(ll, proj4string = CRS("EPSG:4326"))
    #coordinates(ll) <- ~ x.longitude + x.latitude
    #message("Coordinates set")
    #proj4string(ll) <- proj4string(shp_b)
    #st_crs(ll) <- CRS("EPSG:4326")
    #message("projection string set") 
    
    ll_r <- NULL
    if (!is.na(x$transmitter_deployment_longitude[1])) {
      ll_r <-
        data.frame(x$transmitter_deployment_longitude[1], x$transmitter_deployment_latitude[1])

      ll_r <- SpatialPoints(ll_r, proj4string = CRS("EPSG:4326"))
      #coordinates(ll_r) <-
      #  ~ x.transmitter_deployment_longitude.1. + x.transmitter_deployment_latitude.1.
      #message("Step three")
      #proj4string(ll_r) <- suppressWarnings(proj4string(shp_b))
      #st_crs(ll_r) <- CRS("EPSG:4326")
    }
  }
  message("Conversion done.")

  
	## False Detection Algorithm test
  if("FDA_QC" %in% colnames(temporal_outcome))
  {
    write(paste0(x$filename[1],
                 ":  ", " Starting false detection test"),
          file = logfile,
          append = TRUE)
    
    #Trying to force pincock into working as a stopgap.
    x$transmitter_codespace <- x$transmitter_id
    x$receiver_sn <- x$receiver_id
    x$detection_timestamp_utc <- x$detection_datetime
    
    write(paste0(x$filename[1],
                 ":  ", " Stopgap columns set"),
          file = logfile,
          append = TRUE)
    
    temporal_outcome <- qc_false_detection_test(x, temporal_outcome, type = fda_type)

    write(paste0(x$filename[1],
                 ":  ", " False detection test done."),
          file = logfile,
          append = TRUE)
  }
	
  message("False detections done.")
  
	#bathyUrl = "https://upwell.pfeg.noaa.gov/erddap/griddap/etopo5.geotif?ROSE%5B(40):1:(50)%5D%5B(280):1:(320)%5D"
  #message("Starting dist/velocity tests")
	## Distance and Velocity tests
  dist <- NULL
  if(any(is.na(x$transmitter_deployment_latitude)) | any(is.na(x$transmitter_deployment_longitude))) {
    message("Missing some transmitter deployment longitudes/latitudes")
    write(paste0(x$filename[1],
                 ":  ", " Not enough data for some QC checks."),
          file = logfile,
          append = TRUE)
  }
  else
  {
  	position <- data.frame(longitude = c(x$transmitter_deployment_longitude[1], x$longitude),
  		                       latitude = c(x$transmitter_deployment_latitude[1], x$latitude))
  	
    #message("position set")

    #Distance temporarily commented out. We're going to reimplement a lot of this and that includes the shortest_dist calculation, which
    #right now chokes out the rest of the code. So this blows away most of the checks, but it lets the code run so that we can see what
    #happens when the OTN data goes thru it. 
    #dist <- NULL
    # tr is included in the sysdata, but if someone brings their own shapefile then we have to create our own. 
    ## IDJ: add conditional on data_format
    ## BD: added a check to not run the OTN version of this if shp_b is null
  	## IDJ: moved !is.null(shp_b) check inside data_format = otn, otherwise when data_format = imos will never run
  	message("Starting shortest distance calculation.")
  	if(data_format == "otn") {
  	  resolution = scale_meters_to_degrees(transition_layer_res,shp_b,epsg=4326)
  	  #resolution = scale_meters_to_degrees(transition_layer_res,shp_b,ref='max')
  	  message("Resolution calculated.")
  	  transition_layer <- make_transition(st_as_sf(shp_b), res = resolution)
  	  tr <- transition_layer$transition
  	  print("Made transition layer")
  	}
  	dist <- switch(data_format,
  	               imos = {
  	                 shortest_dist2(position,
  	                               x$installation_name,
  	                               rast = Aust_raster,
  	                               tr = tr)
  	               },
  	               otn = {
  	                   shortest_dist2(position,
  	                                 x$installation_name,
  	                                 rast = world_raster,
  	                                 tr = tr)
  	               })
  	message("shortest dist calculated")
  }
 
    if("Velocity_QC" %in% colnames(temporal_outcome) & !is.null(dist)) {
      write(paste0(x$filename[1],
                   ":  ", " Running velocity check"),
            file = logfile,
            append = TRUE)
      
      #Account for custom parameters if we have them.
      if(!is.null(arguments)) {
        velocity_threshold <- arguments['velocity_threshold']
        temporal_outcome <- qc_test_velocity(x, temporal_outcome, dist, velocity_threshold, ...)
      }
      else{
        temporal_outcome <- qc_test_velocity(x, temporal_outcome, dist, ...)
      }
    }
  
    if("Distance_QC" %in% colnames(temporal_outcome) & !is.null(dist)) {
      write(paste0(x$filename[1],
                   ":  ", " Running distance check"),
            file = logfile,
            append = TRUE)
      
      #Account for custom parameters if we have them.
      if(!is.null(arguments)){
        dist_threshold <- arguments['distance_threshold']
        temporal_outcome <- qc_test_distance(x, temporal_outcome, dist, dist_threshold, ...)
      }
      else{
        temporal_outcome <- qc_test_distance(x, temporal_outcome, dist, ...) 
      }
    }

    #Debug to let me know the distance and velocity tests are done running.
		message("Dist/velocity tests done.")

		## Detection distribution test
    if("DetectionDistribution_QC" %in% colnames(temporal_outcome) & !is.null(shp_b)) {
      write(paste0(x$filename[1],
                   ":  ", " Running detection distribution check."),
            file = logfile,
            append = TRUE)
      
      #No custom parameters to worry about here.
      temporal_outcome <- qc_test_det_distro(x, ll, temporal_outcome, shp_b)
    }

    if("DistanceRelease_QC" %in% colnames(temporal_outcome))
    {
      write(paste0(x$filename[1],
                   ":  ", " Running distance from release check."),
            file = logfile,
            append = TRUE)
      
      if(!is.null(arguments)) {
        release_dist_threshold <- arguments['release_dist_threshold']
        temporal_outcome <- qc_test_dist_release(x, temporal_outcome, release_dist_threshold, ...) 
      }
      else {
        temporal_outcome <- qc_test_dist_release(x, temporal_outcome, ...)
      }
    }
		
    if("ReleaseDate_QC" %in% colnames(temporal_outcome)) {
      ## Release date before detection date
      write(paste0(x$filename[1],
                   ":  ", " Running release date check."),
            file = logfile,
            append = TRUE)
      
      #No custom parameters to worry about here. 
      temporal_outcome <- qc_test_release_time_diff(x, temporal_outcome, ...)
    }

    if("ReleaseLocation_QC" %in% colnames(temporal_outcome) & !is.null(dist) & !is.null(shp_b)) {
      write(paste0(x$filename[1],
                   ":  ", " Running release location check."),
            file = logfile,
            append = TRUE)
      
      if(!is.null(arguments)) {
        release_loc_threshold <- arguments['release_loc_threshold']
        temporal_outcome <- qc_release_location_test(x, temporal_outcome, shp_b, dist, ll_r, data_format, release_loc_threshold, ...) 
      }
      else {
        temporal_outcome <- qc_release_location_test(x, temporal_outcome, shp_b, dist, ll_r, data_format, ...) 
      }
    }
		
		## it might be better to keep all tests in temporal_outcome & just ensure
		##  tests that are turned off return NA values, that way output QC object always
		##  has same dims - otherwise this will cause IMOS AODN incoming server checks to
		##  reject QC'd data.

		
		## Detection QC
    temporal_outcome <- qc_detection_qc(temporal_outcome, data_format)
    
	x <- x %>%
	  dplyr::rename(receiver_deployment_longitude = longitude,
	         receiver_deployment_latitude = latitude)
	
	message("Done and returning")
	return(bind_cols(x, temporal_outcome))
}
