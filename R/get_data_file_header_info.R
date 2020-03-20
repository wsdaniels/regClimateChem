get.data.file.header.info <- function(data.struct){

  ###############################################################################
  # This function parses header rows of files pathed in data.struct
  #
  # Input:
  # data.struct: A 1*n list, n = # of files specified. Required fields:
  #   data.struct$file.path
  #   data.struct$file.name
  #
  # Output:
  # column.name.data: A 1*n list, with fields:
  #   column.name.data$column.count[i] is an int reporting inferred column count
  #   column.name.data$column.names[i] is a list of the inferred column names
  ###############################################################################


  ## Verify files are there to check
  if (all(check.data.sources(data.struct)) == F){
    print("Not all file names / file paths exist.")
  }

  ## Open each file, sniff first line, and parse
  nFiles <- length(data.struct[[2]])
  column.name.data <- list()

  for (iFile in 1:nFiles){
    this.file.path <- data.struct$file.path[[iFile]]
    this.file.name <- data.struct$file.name[[iFile]]
    this.file.location <- paste(this.file.path,"/",this.file.name, sep="")
    these.column.names <- read.csv(file=this.file.location, nrows = 1, header = F, stringsAsFactors = F)
    this.column.count <- length(these.column.names)

    column.name.data$column.count <- this.column.count
    column.name.data$column.names <- these.column.names
  }

  ## Return column.name.data
  return(column.name.data)

}
