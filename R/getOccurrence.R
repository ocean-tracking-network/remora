getOccurrence <- function(sciName) {
  #Start by getting the aphiaID from Worms, based on the passed scientific name. 
  aphiaID <- worrms::wm_name2id(name=sciName)
  
  #Now get the GBIF_backboneID, which we'll need in order to get data out of GBIF.
  sourceId <- paste0('urn:lsid:marinespecies.org:taxname:', AphiaID)
  
  #It's hitting an API and getting JSON back, don't overthink it.
  response <- request(base_url = 'https://api.gbif.org/v1/species') %>% 
    req_url_query(`datasetKey` = '2d59e5db-57ad-41ff-97d6-11f5fb264527', 
                  `sourceId` = 'urn:lsid:marinespecies.org:taxname:159222') %>% 
    req_perform() %>%
    resp_body_json()
  
  GBIF_backboneID <- response$results[[1]]$nubKey
  
  #Now get the results from OBIS...
  obis_results <- robis::occurrence(taxonid = AphiaID)
  
  #And the results from GBIF.
  gbif_results <- rgbif::occ_data(taxonKey = GBIF_backboneID) %>%
    .[["data"]]
  
  # Select only the columns we need...
  obis_select <- obis_results %>% 
    select(occurrenceID, 
           decimalLatitude, 
           decimalLongitude) %>% 
    mutate(Source = 'OBIS')
  
  #From both dataframes
  gbif_select <- gbif_results %>% 
    select(occurrenceID, 
           decimalLatitude, 
           decimalLongitude) %>% 
    mutate(Source = 'GBIF')
  
  # Join Data from GBIF and OBIS
  data_joined <- rbind(obis_select,
                           gbif_select)
  
  #And return it to the user. 
  return(data_joined)
}