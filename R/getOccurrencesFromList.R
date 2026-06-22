##' @title Get A Series of Occurrence Files from OBIS and GBIF
##'
##' @description Using a list of species' scientific names, return a list of dataframes containing the occurrence data of the species. This is mostly a wrapper for getOccurrence, I thought it would be more elegant to offload the 'multiple' part
##' to a different function. 
##'
##' @param sciNames The list of scientific names you want to query (if you only supply one name, it will pass it straight through to getOccurrence)
##' @param save Whether or not to save the occurrence files as CSVs locally, for access later.
##' @param saveFolder The local location in which to store occurrence data as CSVs. 
##' 
##' @details Takes the scientific name and uses it to get an AphiaID that can then be used to query OBIS and GBIF. 
##'
##' @return Returns a list of dataframes representing occurrence data for the fish in question. 
##'
##' @importFrom utils write.csv
##' @importFrom stringr str_to_camel
##'
##' @export

getOccurrencesFromList <- function(sciNames, save=FALSE, saveFolder = "localOccurrenceData/") {
    #If someone just passes a single sciName, as though this were getOccurrence, pass it through and return the result. 
    if(is.character(sciNames)) {
      occurrenceOutput <- getOccurrence(sciNames)
    
      if(save == TRUE) {
        fileName = paste0(str_to_camel(sciNames), ".csv")
        write.csv(occurrenceOutput, file=paste0(saveFolder, fileName))
      }
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
        
        #If we're being told to save it all as CSVs...
        if(save == TRUE) {
          fileName = paste0(str_to_camel(sciNames[[index]]), ".csv")
          write.csv(occurrenceOutput[[index]], file=paste0(saveFolder, fileName))
        }
      }
    }
  
    #When we're done, return the list of our occurrence data. 
    return(occurrenceOutput)
}