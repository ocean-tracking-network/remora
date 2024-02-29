get_world_raster <- function(
    world_shape_path,
    species_range
) {
  #We also need a raster for the ocean. We'll load this from a mid-resolution tif file, for testing purposes. 
  world_raster <- raster::raster(world_shape_path)
  #And crop it based on our cropped blue shark extent. 
  world_raster_sub <- raster::crop(world_raster, sf::as_Spatial(species_range))
  
  ## set values to either 1 (ocean) or NA (land)
  world_raster_sub[world_raster_sub < 251] <- NA
  world_raster_sub[world_raster_sub == 251] <- 1
  
  return(world_raster_sub)
}