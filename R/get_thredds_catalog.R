get_thredds_catalog <- function(thredds_uri, catalog, prefix = 'd1') {
  library(thredds)
  library(tools)
  library(ncdf4)
  
  #The thredds package expects to parse XML, so we should make sure that's well and good first. 
  ext <- file_ext(thredds_uri)
  if(ext != "xml") {
    message("The supplied URL does not refer to an XML file. Please supply an XML file.")
  }
  else {
    #Load up a thredds catalogNode for the catalog in question.
    thredds_node <- thredds::CatalogNode$new(thredds_uri, prefix = prefix)
    
    #Pull out the specific catalog that the user specified. 
    thredds_catalog <- thredds_node$get_catalogs(catalog)
    
    #To get a netCDF file out of a catalog opject we need the base URL for the data, plus the URL for the file itself, which is like
    #catalog$get_datasets()[[1]]$url
    #Not the prettiest, but we can work it out. 
    #That gets us a NetCDF file for a dataset, where one exists. 
    #ncvar_get is then the function for getting a file 
    
    return(thredds_catalog)
  }
}