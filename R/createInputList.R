##' @title Create an iterable list of file paths that can be looped over in the Remora notebook. 
##'
##' @description Since the Remora R notebook needs to be able to process a whole folder worth of files rather than a single file, this helper function will produce a list over which the notebook can
##' iterate. This lets us change as little of the underlying structure as possible while still allowing us to do batch processing.
##'
##' @param path The path to either the file or the folder we're going to operate on. 
##'
##' @return Returns either a character vector of filenames or NULL if 'path' is improperly formatted.
##'
##' @export
##' 
createInputList <- function(path) {
  #Set files equal to NULL so if the path is invalid, we return NULL. 
  files <- NULL
  
  #Check to see if the path supplied is that of a directory.
  if(dir.exists(path) && !file.exists(path)) {
    #If so, make 'files' a vector containing the files within the directory. Use the absolute paths. 
    files <- list.files(path, full.names=TRUE)
  }
  #Otherwise, if it's a file, then return the filename in an iterable list.
  else if(file.exists(path)) {
    files <- c(path)
  }
  #If neither of the above is true then we've got a bogus path and should tell the user so. 
  else {
    message("Invalid path supplied: the path doesn't represent either a directory or a file.")
  }
  #In any case, return 'files' so that we can run a loop over it (or, if it's NULL, do something else). 
  return(files)
}