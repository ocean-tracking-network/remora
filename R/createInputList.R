##' @title Create an iterable list of file paths that can be looped over in the Remora notebook. 
##'
##' @description Since the Remora R notebook needs to be able to process a whole folder worth of files rather than a single file, this helper function will produce a list over which the notebook can
##' iterate. This lets us change as little of the underlying structure as possible while still allowing us to do batch processing.
##'
##' @param path The path to either the file or the folder we're going to operate on. 
##'
##' @return Returns either a character vector of filenames or NULL if 'path' is improperly formatted.
##'
##' @importFrom tools file_ext
##'
##' @export
##' 
createInputList <- function(path) {
  #Set files equal to NULL so if the path is invalid, we return NULL. 
  files <- NULL
  
  #Check to see if the path supplied is that of a directory.
  if(dir.exists(path)) {
    #If so, make 'files' a vector containing the files within the directory. Use the absolute paths. 
    files <- list.files(path, full.names=TRUE)
    
    #We need to process the files and make an object that contains a list of all the detection files, as well as the 
    #file for receiver and tag metadata in their own variables, rmeta and tmeta. 
    #At this time we are assuming that a given folder corresponds to one project, and so all 
    #receiver/tag metadata files can be merged into single receiver/tag metadata files, which we can then pass thru to
    #every QC run. 
    
    
    #These will hold data as we go, but the thing we return is going to be a vector containing the list of detection filenames plus the dataframes for receiver and tag metadata.
    #We're including the metadata as dataframes because we already have to open and read in the files as we go anyway, so we might as well aggregate them here rather than elsewhere.
    dets <- c()
    rmeta <- NULL
    tmeta <- NULL
    
    for(filename in files) {
      #If it's a parquet file, then we know immediately it's a detections file. 
      if(file_ext(filename) == "parquet") {
        message("Parquet File", filename)
        dets <- append(dets, filename)
      }
      
      #Otherwise, we need to check it out- as long as it's a CSV or XLSX file.
      else {
        if(file_ext(filename) == "csv") {
          #If it's CSV data then it's detection data and we can interpret it as such. 
          message("File ", filename, " interpreted as detection data.")
          dets <- append(dets, filename)
        }
        else if(file_ext(filename) == "xlsx" || file_ext(filename) == "xls"){
          #If it's an excel file then we need to determine if it's Tag or Receiver metdata.
          
          #Leaving this here as a reminder to myself that we will need to account for the possibility of multiple sheets containing tagging metadata.
          #sheets <- excel_sheets(filename)
          
          #We can't account for every conceivable name that someone might use for their sheet, so we're going to adapt a version of the shortform processing code to iterate over the first ~10 rows to find the headers,
          #which we can then use to determine what kind of file this is. 
          skip_rows = 0
          num_of_skips = 10
          
          while(skip_rows < num_of_skips) {
            #Read the data and grab the column names. 
            data = read_excel(filename, sheet=2, skip=skip_rows,  na = c("", "null", "NA"))
            columnset = colnames(data)
            
            #If INS_MODEL_NO is in the columns, then we have receiver metadata.
            if('INS_MODEL_NO' %in% columnset) {
              message("File ", filename, " interpreted as receiver metadata.")
              rmeta <- processMeta(data, rmeta)
              break
            }
            #If TAG_TYPE is in the column names, then it's tag metadata.
            else if("TAG_TYPE" %in% columnset) {
              message("File ", filename, " interpreted as tag metadata.")
              tmeta <- processMeta(data, tmeta)
              break
            }
            #Otherwise, iterate over to the next phase of the loop. 
            else {
              skip_rows <- skip_rows + 1 
            }
          }
        }
        else {
          #If a file is neither CSV nor XLS(X), let the user know we couldn't do anything with it. 
          message("File ", filename, " could not be interpreted and was ignored.")
        }
      }
    }
    
    return_info <- list("dets" = dets, "rmeta" = rmeta, "tmeta" = tmeta)
    return(return_info)
  }
  #Otherwise, if it's a file, then return the filename in an iterable list.
  else if(file.exists(path)) {
    files <- c(path)
    return(files)
  }
  #If neither of the above is true then we've got a bogus path and should tell the user so. 
  else {
    message("Invalid path supplied: the path doesn't represent either a directory or a file.")
    return(NULL)
  }
  #In any case, return 'files' so that we can run a loop over it (or, if it's NULL, do something else). 
  return(files)
}

#Helper function to do the ongoing aggregation of metadata from the function above. "Data" is the data to be added, while "metadata" in the object to which it will be added.
processMeta <- function(data, metadata) {
  #If metadata is null (i.e, hasn't been created yet) then we can return data and call it good.
  if(is.null(metadata)) {
    return(data)
  }
  
  #Otherwise, we have to aggregate data and metadata.
  else {
    #Bind them together.
    metadata <- rbind(metadata, data)
    
    #Get rid of any duplicates.
    metadata <- metadata[!duplicated(metadata), ]
    
    #Return the newly aggregated metadata.
    return(metadata)
  }
  #Cheeky little return juuuust in case.
  return(NULL)
}