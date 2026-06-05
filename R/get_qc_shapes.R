##' @title Generate the polygon for a species' home range for use in the QC run.
##' 
##' @description Wrap a call to getOccurrence and createPolygon so that the user can supply all their parameters at once and get the polygon back in one call. 
##' 
##' @param scientificName The scientific name of the animal
##' @param fraction passed directly through to createPolygon
##' @param partsCount passed directly through to createPolygon
##' @param buff passed directly through to createPolygon
##' @param clipToCoast passed directly through to createPolygon
##' @param bounds passed directly through to createPolygon
##' 
##' @return The polygon representing the species home range. 
##' 
##' @export

get_qc_shapes <- function(scientificName, 
                          fraction=1, 
                          partsCount=1, 
                          buff=500000, 
                          clipToCoast = "aquatic", 
                          bounds=bounds) {
  

  #Start by getting the occurrence data.
  speciesOccurrence <- getOccurrence(scientificName)
  
  #Once we have that, generate the polygon info. 
  speciesList <- createPolygon(speciesOccurrence, fraction=1, partsCount=1, buff=500000, clipToCoast = "aquatic", bounds=bounds)
  
  #At this time we don't use speciesVector for anything, but it's being generated so I'll leave it here in case we need it for something later. 
  speciesVector <- speciesList$vector
  
  #Here's the polygon we want. 
  speciesPolygon <- speciesList$polygon
  
  return(speciesPolygon)
}