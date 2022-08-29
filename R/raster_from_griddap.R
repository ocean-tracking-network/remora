raster_from_griddap <- function(griddap, var = NULL) {
  #Get the filepath to the saved netcdf file associated with the griddap object.
  filename <- griddap$summary['filename']
  
  #If we don't have a var supplied, then brick will operate without one. In practice this means
  #that it will use the only one present (if there's only one), or guess if there are multiple variables.
  if(is.null(var)) {
    griddap_brick <- brick(filename)
  }
  #Otherwise, we can explicitly specify which variable we want to associate with the raster. 
  else {
    griddap_brick <- brick(filename, varname = var)
  }
  
  #Now we can return the raster brick, which can be used as the background for a plot. 
  return(griddap_brick)
}