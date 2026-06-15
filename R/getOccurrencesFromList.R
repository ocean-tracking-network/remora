##' @title Get A Series of Occurrence Files from OBIS and GBIF
##'
##' @description Using a list of species' scientific names, return a list of dataframes containing the occurrence data of the species. This is mostly a wrapper for getOccurrence, I thought it would be more elegant to offload the 'multiple' part
##' to a different function. 
##'
##' @param sciNames The list of scientific names you want to query (if you only supply one name, it will pass it straight through to getOccurrence)
##' 
##' @details Takes the scientific name and uses it to get an AphiaID that can then be used to query OBIS and GBIF. 
##'
##' @return Returns a list of dataframes representing occurrence data for the fish in question. 
##'
##' @importFrom httr2 resp_body_json
##' @importFrom worrms wm_name2id
##' @importFrom robis occurrence
##' @importFrom rgbif occ_data
##' @export

getOccurrencesFromList <- function(sciNames) {
    #If someone just passes a single sciName, as though this were getOccurrence, pass it through and return the result. 
    if(is.character(sciNames)) {
      return(getOccurrence(sciNames))
    }
    #Otherwise, we have to operate on the list itself. 
    else {
      #Make a list to hold our output. 
      occurrenceOutput <- list()
      #Iterate over the list (easier to use the index here since it lets us populate the list more cleanly.)
      for(index in seq_along(sciNames)) {
        #Get the occurrence and add it to the list.
        message(sciNames[[index]])
        occurrenceOutput[[index]] <- getOccurrence(sciNames[[index]])
      }
      
      #When we're done, return the list of our occurrence data. 
      return(occurrenceOutput)
    }
}