##' @title Create Polygon 
##'
##' @description Create a species range polygon for QC tests from species occurence data
##'
##' @param occurenceFile ...
##' @param fraction ...
##' @param buffer ...
##' @param partCount ...
##' @param coordHeaders ...
##' @param clipToCoast ...
##' @param returnWhole ...
##' 
##' @details Creates species range polygon for QC tests
##'
##' @return retruns a polygon data frame
##'
##'
##' @importFrom dplyr '%>%' filter
##' @importFrom readr read_csv
##' @importFrom rangeBuilder getDynamicAlphaHull
##'
##' @export

#Function made by Bruce Delo but the underlying code was developed by Jessica Castellanos (UGuelph). Many of the comments are hers too.

createPolygon <- function(occurrences, 
                          fraction = 0.70, 
                          buffer = 1000, 
                          partsCount = 20,  
                          coordHeaders = c("decimallongitude", "decimallatitude"), 
                          clipToCoast = "aquatic",
                          bounds = NULL) {
  
  if(typeof(occurrences) == "character") {
    #Read in the occurrence CSV. 
    occurrence <- read_csv(occurrences)
  }
  else {
    occurrence <- occurrences
  }
  
  #If bounds have been provided, use them to slim down the occurrences. 
  if(!is.null(bounds)) {
    occurrence <- occurrence[occurrence$decimalLatitude > bounds$minLat & 
                                      occurrence$decimalLatitude < bounds$maxLat &
                                      occurrence$decimalLongitude > bounds$minLon &
                                      occurrence$decimalLongitude < bounds$maxLon,]
  }
  
  #Using the R package rangeBuilder to generate an alpha hull polygon which defines a concave hull or boundary around a set of points in two or three dimensions. 
  #The alpha hull polygon is a generalization of the convex hull, allowing for the creation of concave regions. 
  #The function getDynamicAlphaHull integrates the function ahull, which calculates the α-convex hull of a given sample of points in the plane for α >0. 
  #It also includes other parameters of relevance, like the minimum fraction of occurrences that must be included in a polygon, the maximum number of disjunct polygons allowed, and setting a buffer zone.
  
  #if("flags" %in% colnames(occurrence)) {
  # occurrence <- occurrence %>% filter(is.na(flags))
  #}
  
  #Using the function getDynamicAlphaHull to generate a polygon based on occurrence data. In this case, the parameters represent:
  #fraction: the minimum fraction of occurrences that must be included in the polygon - 70%
  #buff: buffering distance in meters - 1000. This should be adjusted considering OBIS buffering
  #partCount: the maximum number of disjunct polygons that are allowed. I set this to a high number to allow for global distribution and multiple polygons.
  #clipToCoast: Either "no" (no clipping), "terrestrial" (only terrestrial part of the range is kept) or "aquatic" (only non-terrestrial part is clipped).
  
  #EDIT: I found a different package, voluModel, that wraps the getDynamicAlphaHull function with a bunch of extra greeblies specifically for converting occurrence data into
  #shapefiles for oceangoing species. Subbed that in here, but most of the parameters are the same. ClipToOcean is added by marineBackground and will automatically drop any chunks of the
  #polygon that don't contain actual occurrences and are just generated as artefacts of the hull-making process.
  
  occurrenceVector <- voluModel::marineBackground(occurrence, 
                                  fraction = fraction, 
                                  buff = buffer, 
                                  partCount = partsCount, 
                                  coordHeaders = coordHeaders, 
                                  clipToCoast = clipToCoast,
                                  clipToOcean = TRUE)
  
  message("got this far")
  View(occurrenceVector)
  
  polygon <- st_as_sf(occurrenceVector)
  
  #I'm returning both here because a recent update to the glatos package made it so that we can't pass the multipolygon to make_transition (the function we had been using, make_transition2,
  #got deprecated). Parts of the code may still rely on the multipolygon, which is why I'm returning it here, but adding the occurrenceVector gives us latitude to transform it into other
  #spatial objects if more functions like make_transition need something other than spatVectors and multipolygons. Depending on how the rest of the code shakes out I may make this a toggle
  #that you can control with an argument to return one, the other, or both. 
  return(c("vector" = occurrenceVector, "polygon" = polygon))
}


