#' @title Fetch species home range polygons if they exist locally, create and store them if not.
#' 
#' @import stringr str_to_camel
#' @import sf write_sf
#' @export


createAndFetchPolygons <- function(sciNames, occurrenceFolder="localOccurrenceData/", polygonFolder="localPolygonData/") {
  #We're going to iterate over the list of scientific names that we get and ultimately return a list of appropriate species data polygons. If they don't exist locally, they'll be created and saved before being returned. Later on, if they
  #already exist, we'll just load them rather than creating them from scratch.
  polygons <- list()
  for(index in seq_along(sciNames)) {
    
    scientificName = str_to_camel(sciNames[index])
    polygonFilename = paste0(polygonFolder, scientificName, '.geojson')
    occurrenceFilename = paste0(occurrenceFolder, scientificName, '.csv')
    
    print("Scientific Name")
    print(scientificName)
    
    #First we check and see if a polygon exists. 
    if(file.exists(polygonFilename)){
      message(paste0("Matching polygon found for ", scientificName, "."))
      
      speciesPolygon <- read_sf(polygonFilename)
      
      polygons[[scientificName]] <- speciesPolygon$polygon
    }
    
    #If not that, then we'll check to see if an occurrence CSV exists.
    else if(file.exists(occurrenceFilename)){
      message(paste0("No polygon found, but matching occurrence data found for ", scientificName, "."))
      #Load the occurrence data as a dataframe. 
      occurrenceData <- read.csv(occurrenceFilename)
      
      #Pass it to createPolygon. 
      occurrenceList <- createPolygon(occurrenceData, fraction=1, partsCount=1, clipToCoast = "aquatic")
      
      #When that's done, we'll save the vector itself in the polygons folder...
      write_sf(occurrenceList$polygon, polygonFilename)
      
      #...and add the polygon to our output list. 
      polygons[[scientificName]] <- occurrenceList$polygon 
    }
    
    #If none of that pops off then we have to get the occurrence data from scratch and make the polygon.
    else {
      message(paste0("No matching data found for ", scientificName, ". Deriving occurrence data from OBIS and GBIF and creating polygon. Be aware that this may take a long time."))
      
      speciesOccurrence <- getOccurrence(sciNames[[index]])
      
      #Save the occurrence data.
      write.csv(speciesOccurrence, occurrenceFilename)
      
      speciesList <- createPolygon(speciesOccurrence, fraction=1, partsCount=1, clipToCoast = "aquatic")
      
      #When that's done, we'll save the vector itself in the polygons folder...
      write_sf(speciesList$polygon, polygonFilename)
      
      #...and add the polygon to our output list. 
      polygons[[scientificName]] <- speciesList$polygon 
    }
  }
  
  return(polygons)
}  