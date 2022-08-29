match_env_to_det <- function(dets, dataset, var_name, url = NULL, 
                             lon_column = "longitude", lat_column = "latitude", date_column = "datecollected", z_column = NULL) {
  library(rerddapXtracto)
  library(rerddap)
  library(plotdap)
  library(ggplot2)
  
  if(is.null(url)){
    url <- rerddap::eurl()
  }
  
  dataset_info <- rerddap::info(dataset)
  
  xpos <- dets[[lon_column]]
  ypos <- dets[[lat_column]]

  dates <- parse_date_time(dets[[date_column]], orders = c('ymd', 'ymd HMS'), tz = "UTC")
  tpos <- dates
  
  zpos <- NULL
  if(!is.null(z_column)) {
    zpos <- dets[[z_column]]
  }
  
  zpos <- rep(0., length(xpos))
  
  extract <- rxtracto(dataset_info, parameter = var_name, 
                      xcoord = xpos, ycoord = ypos, tcoord = tpos, zcoord = zpos,
                      xlen = .2, ylen = .2, progress_bar = TRUE)
  
  return(extract)
  
}

match_and_plot <- function(dets, dataset, var_name, url = NULL, 
                           lon_column = "longitude", lat_column = "latitude", date_column = "datecollected", z_column = NULL) {
  library(plotdap)
  
  extract <- match_env_to_det(dets, dataset, var_name, url, lon_column, lat_column, date_column, z_column)
  
  plot <- plotdap(extract, dets[[lon_column]], dets[[lat_column]], 
                  parse_date_time(dets[[date_column]], orders = c('ymd', 'ymd HMS'), tz = "UTC"))
  
  return(plot)
}