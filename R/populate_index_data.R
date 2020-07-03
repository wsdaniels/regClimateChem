populate.index.data <- function(specified.index.data){

  ###############################################################################
  # This function populates data fields in filled.index.data list based
  # on filename and filepath in specified.index.data.
  #
  # File at filepath\filename must be a csv file with a header
  # row and two columns (the first a time index in YYYMMDD format, the second
  # the anomaly value of the climate mode index of interest).
  #
  # For example:
  #   aao_month_avg.csv
  #   -----------------
  #   "time","anomaly"
  #   19790201,0.35626,
  #   19790301,0.89918,
  #
  # Input:
  # specified.index.data: A 1*n list. Required fields:
  #   specified.index.data$file.path
  #   Specified.index.data$file.name
  #
  # Output:
  # filled.index.data: A copy of specified.index.data with additional fields from file:
  #   filled.index.data$year.month.day
  #   filled.index.data$anomaly
  ###############################################################################


  # Prepopulate
  filled.index.data <- specified.index.data

  # Determine number of files to read
  nIndices <- length(specified.index.data[[1]])

  # Check that index data sources exist
  if(all(check.data.sources(specified.index.data))){

    for (iIndex in 1:nIndices){
      # Read from file
      this.file.path <- specified.index.data$file.path[iIndex]
      this.file.name <- specified.index.data$file.name[iIndex]
      this.file.location <- paste(this.file.path,"/",this.file.name, sep="")

      this.data.array <- read.csv(this.file.location)

      if (all(is.na(this.data.array$anomaly)) == T){
        this.data.array$anomaly <- this.data.array$time
        this.data.array$time <- row.names(this.data.array)
        row.names(this.data.array) <- 1:length(this.data.array$time)
      }

      filled.index.data$year.month[[iIndex]] <- this.data.array$time
      filled.index.data$anomaly[[iIndex]] <- this.data.array$anomaly

    }
  }
  else{
    print("Location of index data not found.")
  }

  ## Return filled.index.data
  return(filled.index.data)
}
