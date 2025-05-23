##' @title QC Distance test
##'
##' @description ...
##' 
##' @param data data formatted by either `remora::get_data` or `remora::get_data_arbitrary`
##' @param qc_results the `temporal_outcome` object, holding all QC test results, 
##' generated by `remora::qc`
##' @param dist output from `remora::shortest_dist`
##' @param dist_threshold ...
##' 
##' @details ...
##' 
##' @return adds Distance test outcomes to temporal_outcome object
##' 
##' @keywords internal
##' 
qc_test_distance <- function(data, 
                             qc_results, 
                             dist, 
                             ...) {
  if (!exists("dist_threshold")) {
    dist_threshold <- 1000
  }
  
  if (length(dist) == 1) {
    qc_results["Distance_QC"] <- ifelse(dist <= dist_threshold, 1, 2)
  } 
  else if (length(dist) > 1) {
    dist_next <- c(dist[2:nrow(dist)], NA)
    
    ## Distance test
    qc_results[, "Distance_QC"] <- ifelse(dist > dist_threshold & dist_next > dist_threshold, 2, 1)
    qc_results[1, "Distance_QC"] <- ifelse(dist[1] > dist_threshold, 2, 1)
    qc_results[nrow(data), "Distance_QC"] <- ifelse(dist[nrow(data)] > dist_threshold, 2, 1)
  }
  
  return(qc_results)
}