##' title Get Species Range Polygon
##'
##' @description Takes an occurrence file (usually from OBIS) representing a fish's home range, and converts it into
##' an alpha hull that can function as a polygon for QC tests in Remora that require it. 
##' 
##' @param occurrence_path The path to the occurrence file
##' @param occurrence_nas Optional parameter to properly replace NAs in the occurrence file
##' @param fraction Pass-through to getDynamicAlphaHull, see that doc for info
##' @param buff Same as fraction
##' @param partCount Same as fraction
##' @param coordHeaders same as Fraction
##' @param clipToCoast same as fraction
##' 
##' 
##' @return The output from getDynamicAlphaHull, which contains a polygon and alpha channel. 
##' 
##' @importFrom readr read_csv
##' @importFrom rangebuilder getDynamicAlphaHull
##' @export

getSpeciesRangePolygon <- function(
    occurrence_path,
    occurrence_nas = c("", "NA", "{}"),
    fraction = 0.7,
    buff = 1000, 
    partCount = 20, 
    coordHeaders = c("decimallongitude", "decimallatitude"), 
    clipToCoast = "aquatic"
) {
  occurrence <- read_csv(occurrence_path, na = occurrence_nas)
  occurrencePolygon <- getDynamicAlphaHull(
    occurrence, 
    fraction = fraction, 
    buff = buff, 
    partCount = partCount, 
    coordHeaders = coordHeaders, 
    clipToCoast = clipToCoast
  )
  
  return(occurrencePolygon)
}