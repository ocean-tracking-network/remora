##' title Release Location test
##'
##' @description ...
##' 
##' @param data data formatted by either `remora::get_data` or `remora::get_data_arbitrary`
##' @param qc_result the `temporal_outcome` object, holding all QC test results, 
##' generated by `remora::qc`
##' @param species_range the expert species distribution shapefile (if avaiable)
##' @param distances ...
##' @param latlons ...
##' 
##' @details ...
##' 
##' @return adds Detection Distribution test outcomes to temporal_outcome object
##' 
##' @importFrom sf as_Spatial
##' @keywords internal
##' 
qc_release_location_test <-
  function(data,
           qc_result,
           species_range,
           distances,
           latlons,
           data_format,
           ...) {

    if(!exists("release_loc_threshold")) {
      release_loc_threshold <- 500
    }
        
    message("Starting release location test.")
    ## Release location test
    if (!is.null(species_range)) {
      species_range_spatial <- sf::as_Spatial(species_range)
      
      #Get the first nonzero result in the distances matrix, representing the first detection's distance from the receiver. 
      nonzero <- which(distances!=0)
      
      #Also get the intersection of the release lat/lon and the species polygon.
      intersect_val <- st_intersects(st_as_sf(latlons), st_as_sf(species_range))
      #The above returns a slightly weird value- a list, containing either 1 (if the intersection is true) or a zero-length integer (if the intersection is false). 
      #That's why we have to check with 'identical()' below- it's a good way to compare something to integer(0). 
      
      qc_result[, "ReleaseLocation_QC"] <-
          ifelse(
            #Two criteria must be met for the test to fail:
            all(
              #The distance between the release and the first detection must be greater than the given distance threshold...
              distances[nonzero] > release_loc_threshold, 
              
              #And the release must fall outside of the species home range polygon. 
              identical(intersect_val[[1]], integer(0))), 
            
              #If both are false, then the test fails (i.e, receives a 2), if either (or both) is true, the test passes. 
              2, 1
          )
    } else {
      qc_result[, "ReleaseLocation_QC"] <-
        ifelse(distances[1] > release_loc_threshold, 2, 1)
    }
    message("release location test done")
    return(qc_result)
  }