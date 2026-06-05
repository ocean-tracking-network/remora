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
    
    
    #These will hold data as we go, but the thing we return is going to be a vector containing the list of detection filenames plus the filenames for the aggregated tag/receiver files. 
    dets <- c()
    rcvr <- c()
    tag <- c()
    ignored <- FALSE
    
    for(filename in files) {
      message(filename)
      #If it's a parquet file, then we know immediately it's a detections file. 
      if(file_ext(filename) == "parquet") {
        message("Parquet File", filename)
        dets <- append(dets, filename)
      }
      
      #Otherwise, we need to check it out- as long as it's a CSV file. (Do we also need to handle Excel?)
      else if(file_ext(filename) == "csv") {
        #Open the first line of the file.
        row <- read.csv(filename, na = c("", "null", "NA"))
        
        View(row)
        
        #Now check to see which indicator columns are present.
        
        #If INS_MODEL_NO is present, it's receiver metadata.
        if ("INS_MODEL_NO" %in% names(row)) {
          rcvr <- append(rcvr, filename)
        }
        else if("TAG_TYPE" %in% names(row)) {
          tag <- append(tag, filename)
        }
        else {
          dets <- append(dets, filename)
        }
      }
      
      #If we encounter a file that isn't either of the above we'll set 'ignored' to TRUE and at the end we'll
      #Let the user know that we have ignored some files on the basis of their extensions. Handy diagnostic.
      else {
        ignored <- TRUE
      }
    }
    
    View(rcvr)
    View(tag)
    
    #Having processed all our filenames, we now need to aggregate tags and receivers into single files (each). 
    tag_return = processMeta(tag)
    
    #Now we do the same for the receiver data.
    rcvr_return = processMeta(rcvr)
    
    return_info <- list("dets" = dets, "rcvr" = rcvr_return, "tags" = tag_return)
    View(return_info)
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

processMeta <- function(metaList) {
  if(length(metaList) == 1) {
    return(metaList[[1]])
  }
  else {
    return_frame <- data.frame()
    
    for(file in metaList) {
      if(file_ext(file) == "csv"){
        metadata <- read.csv(file)
      }
      else if (file_ext(file) == "xls" || file_ext(file == "xlsx")){
        metadata <- read_excel(file)
      }
      
      rbind(return_frame, metadata)
    }
    
    return_frame <- return_frame[!duplicated(return_frame), ]
    return(return_frame)
  }
  return(NULL)
}