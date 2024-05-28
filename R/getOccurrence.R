##' @title Get Occurrence File from OBIS and GBIF
##'
##' @description Using a species' scientific name, get a CSV representing occurrence data from the combined datasets of OBIS and GBIF
##'
##' @param sciName The scientific name of the species you want to query. 
##' 
##' @details Takes the scientific name and uses it to get an AphiaID that can then be used to query OBIS and GBIF. 
##'
##' @return Returns a csv representing occurrence data for the fish in question. 
##'
##' @import httr2 resp_body_json
##' @import worrms wm_name2id
##' @export

getOccurrence <- function(sciName) {
  #Start by getting the aphiaID from Worms, based on the passed scientific name. 
  aphiaID <- worrms::wm_name2id(name=sciName)
  
  #Now get the GBIF_backboneID, which we'll need in order to get data out of GBIF.
  sourceId <- paste0('urn:lsid:marinespecies.org:taxname:', aphiaID)
  
  #It's hitting an API and getting JSON back, don't overthink it.
  response <- request(base_url = 'https://api.gbif.org/v1/species') %>% 
    req_url_query(`datasetKey` = '2d59e5db-57ad-41ff-97d6-11f5fb264527', 
                  `sourceId` = paste0('urn:lsid:marinespecies.org:taxname:', aphiaID)) %>% 
    req_perform() %>%
    resp_body_json()
  
  GBIF_backboneID <- response$results[[1]]$nubKey
  
  #Now get the results from OBIS...
  obis_results <- robis::occurrence(taxonid = aphiaID)
  
  #And the results from GBIF.
  gbif_results <- rgbif::occ_data(taxonKey = GBIF_backboneID) %>%
    .[["data"]]
  
  # Select only the columns we need...
  obis_select <- obis_results %>% 
    dplyr::select(occurrenceID, 
           decimalLatitude, 
           decimalLongitude) %>% 
    dplyr::mutate(Source = 'OBIS')
  
  #From both dataframes
  gbif_select <- gbif_results %>% 
    dplyr::select(occurrenceID, 
           decimalLatitude, 
           decimalLongitude) %>% 
    dplyr::mutate(Source = 'GBIF')
  
  # Join Data from GBIF and OBIS
  data_joined <- rbind(obis_select,
                           gbif_select)
  
  #And return it to the user. 
  return(data_joined)
}