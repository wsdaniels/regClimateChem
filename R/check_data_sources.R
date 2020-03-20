check.data.sources <- function(data.struct){

  ###############################################################################
  # This function verifies that files exist at the paths in the list.
  #
  # Input:
  # data.struct: A 1*n list, n = # of data sources specified. Required fields:
  #   data.struct$file.path
  #   data.struct$file.name
  #
  # Output:
  # is.file: A 1*n logical, where is.file[i] == true if a file actually exists
  # at the location specified in data.struct$file.path[i] and 
  # data.struct$file.name[i].
  ###############################################################################
  
  ## Check if path/file specification actually points to a file
  nFiles <- length(data.struct[[2]])
  if (class(data.struct) == "data.frame"){
    nFiles <- length(data.struct)
  }
  is.file <- rep(F,nFiles)
  
  for (iFile in 1:nFiles){
    this.file.path <- data.struct$file.path[[iFile]]
    this.file.name <- data.struct$file.name[[iFile]]
    this.file.location <- paste(this.file.path,"/",this.file.name, sep="")
    if (file.exists(this.file.location) == T){
      is.file[iFile] <- T
    }
  }
  
  return(is.file)
  
}