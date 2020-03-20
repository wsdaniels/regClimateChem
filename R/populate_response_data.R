populate.response.data <- function(specified.response.data) {

  ###############################################################################
  # This function populates data fields in filled.response.data struct based
  # on filename and filepath in specified.response.data. Input data need not
  # be sorted; output struct will be sorted ascending.
  #
  # File at filepath\filename must be a csv file, with a header
  # row, and at least two columns.
  # The first column must be a time index in YYYYMM format.
  # The column indicated in the data.column.index field is imported as response
  # data. An example of an acceptable file format is shown below.
  #
  # MaritimeSEA_V7TMOPITTanomalies.csv
  # -----------------
  # "time","tcol_co","anomaly_co"
  # 200101,1.73973e+18,-1.27492e+17
  # 200102,2.08868e+18,1.34739e+17
  #
  # Input:
  # specified.response.data: A list. Required fields:
  #   specified.response.data$file.path
  #   specified.response.data$file.name
  #   specified.response.data$data.column.index
  #
  # Output:
  # filled.response.data a copy of specified.response.data with additional
  # fields from file:
  #   filled.response.data$year.month
  #   filled.response.data$response.var
  #   filled.response.data$column.name
  ###############################################################################

  ## Prepopulate
  filled.response.data <- specified.response.data

  ## Get basic file header info
  column.name.data <- get.data.file.header.info(filled.response.data)

  ## Check if user specified data.column.index is allowed
  this.column.index <- specified.response.data$file.data.column.index
  min.column.index <- 2L
  max.column.index <- column.name.data$column.count

  is.column.index.valid <- this.column.index >= min.column.index & this.column.index <= max.column.index
  if (is.column.index.valid == F){
    print("User-specified data column index invalid. Check fileDataColumnIndex field and source file format.")
  }

  ## Get and save column name from header
  filled.response.data$data.column.name = column.name.data$column.names[[this.column.index]]

  ## Get the input
  this.file.path <- specified.response.data$file.path
  this.file.name <- specified.response.data$file.name
  this.file.location <- paste(this.file.path,"/",this.file.name, sep="")

  this.data.array <- read.csv(this.file.location)

  ## Make sure the data is sorted by time
  this.data.array[order(this.data.array$anomaly_co),]
  filled.response.data$year.month <- list(this.data.array[,1])
  filled.response.data$response.var <- list(this.data.array[,this.column.index])

  ## Return filled.response.data
  return(filled.response.data)
}
