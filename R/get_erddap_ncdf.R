##' @title get_erddap_ncdf
##' @description Query an ERDDAP server (supplied by the user) for a specific variable, which can later be associated with
##' detection data. The user passes in their entire dataframe, from which lat/lon and date information is then
##' derives. They also supply the url, file, and layer that they want to query against. The function then returns
##' the result of a griddap() call with those parameters, which will contain summary information about the 
##' ncdf target file as well as the gridded data. 
##'
##' @param url The url to the ERDDAP server the user wants to query. 
##' @param file The filename on the server that the user wants to search. 
##' @param layer The variable in the file that the user wants to query. 
##' @param df The dataframe containing the date, lat, and lon information the user wants to query against. This can be their entire detection dataframe or only date, lat, and lon columns. 
##' @param date_col Optional parameter if the name of the date column in the dataframe is different from the OTN detection extract standard. 
##' @param lat_col Optional parameter if the name of the latitude column in the dataframe is different from the OTN detection extract standard. 
##' @param lon_col Optional parameter if the name of the longitude column in the dataframe is different from the OTN detection extract standard. 
##'
##' @return a tibble containing the ncdfopen data regarding the target NetCDF file and the gridded data associated with the time and position the user wanted to query. 

get_erddap_ncdf <- function(url, file, layer, df, date_col = "datecollected", lat_col = "latitude", lon_col = "longitude") {
  library(lubridate)
  
  #calculate the study extent. 
  minLat <- min(df[lat_col])
  maxLat <- max(df[lat_col])
  minLon <- min(df[lon_col])
  maxLon <- max(df[lon_col])
  
  #Calculate the date extent for our call to griddap. (Make sure to use double square brackets so we get a vector and not a tibble.)
  dates <- parse_date_time(df[[date_col]], orders = c('ymd', 'ymd HMS'), tz = "UTC")
  minDate <- min(dates)
  maxDate <- max(dates)
  
  grid <- griddap(
   file,
   latitude = c(minLat, maxLat),
   longitude = c(minLon, maxLon),
   time = c(minDate, minDate %m+% months(12)),
   fields = layer,
   url = url
  )
  
  return(grid)
}