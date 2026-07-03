##' @title From an OTN-style detection extract, compile the scientific names and load/create/cache appropriate polygons representing home ranges. 
##'
##' @description This function wraps other polygon generation code. Given a detection extract (and, if necessary, appropriate nonstandard column name,
##') it will iterate over the scientific names and create polygons as it goes- loading them if they're already cached, caching them if they're not. 
##'
##' @param extract_path A path to the detection extract. 
##' @param scientificNameColumn If for whatever reason the column your scientific names are stored in isn't 'scientificName',
##' you can specify a different one. 
##' 
##' @return A list of polygons that can be passed on to runQC. 
##'
##' @importFrom dplyr distinct
##'
##' @export

getPolygonsFromDetectionExtract <- function(extract_path, scientificNameColumn = "scientificName") {
  #Load in the file. This is going to need to be using Surimi but for now I'll just leave it local. 
  data <- load_file(extract_path)
  
  #Get the list of the scientific names present in the detection extract.
  scientificNameList <- distinct(data[scientificNameColumn])
  
  speciesPolygons <- createAndFetchPolygons(scientificNameList$scientificName)
  
  return(speciesPolygons)
}