getPolygonsFromDetectionExtract <- function(extract_path, scientificNameColumn = "scientificName") {
  #Load in the file. This is going to need to be using Surimi but for now I'll just leave it local. 
  data <- load_file(extract_path)
  
  #Get the list of the scientific names present in the detection extract.
  scientificNameList <- distinct(data[scientificNameColumn])
  
  speciesPolygons <- createAndFetchPolygons(scientificNameList$scientificName)
  
  return(speciesPolygons)
}