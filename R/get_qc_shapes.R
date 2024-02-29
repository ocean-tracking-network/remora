##' @title generate shapefiles and transition layers
##' 
##' @description generate shapefiles and transition layers for QC based on user-supplied parameters
##' 
##' @param detection_extract Either a path to a detection extract file, or a dataframe representing the detection extract.
##' @param shapefile_path Path to an OBIS occurrence file from which a shapefile can be derived with getDynamicAlphaHull
##' @param crop Whether to use the entire shapefile or crop it to the boundaries derived from the detection data. Defaults to True.
##' 
##' @return a list of cropped shapefile, transition layer, and world raster
##' 
##' @importFrom sf st_crop st_bbox
##' @importFrom raster raster crop
##' @importFrom rangeBuilder getDynamicAlphaHull
##' 
##' @keywords internal

get_qc_shapes <- function(detection_extract,
                          shapefile_path,
                          crop = TRUE)  {
  
  #If the detection extract has been passed as a filepath rather than a dataframe, then just read it in. 
  if (file.exists(detection_extract)) {
    detection_extract <- read_csv(detection_extract)
  }
  
  #Get the shapefile from the shapefile path:
  species_range_raw <- read_csv(shapefile_path)
  species_range <- species_range_raw %>% filter(is.na(flags))
  #Gotta bust these args up to the user level.
  species_polygon_hull <- getDynamicAlphaHull(species_range, fraction = 0.70, buff = 1000, partCount = 20, coordHeaders = c("decimalLongitude", "decimalLatitude"), clipToCoast = "aquatic")
  species_polygon <- species_polygon_hull[[1]]
  
  if (crop) {
    if(!inherits(detection_extract, "sf")) {
      minLat = min(detection_extract$latitude) - 5
      minLon = min(detection_extract$longitude) - 5
      maxLat = max(detection_extract$latitude) + 5
      maxLon = max(detection_extract$longitude) + 5
      #Crop the range shapefile
      species_polygon <- st_crop(species_polygon,  xmin=minLon, ymin=minLat, xmax=maxLon, ymax=maxLat)
    } else {
      ext <- st_bbox(detection_extract)
      ext[1] <- ext[1] + 5 * sign(ext[1])
      ext[2] <- ext[2] + 5 * sign(ext[2])
      ext[3] <- ext[3] + 5 * sign(ext[3])
      ext[4] <- ext[4] + 5 * sign(ext[4])
      #Crop the range shapefile
      species_polygon <- st_crop(species_polygon, ext)
    }
  }
  
  return (species_polygon)
}