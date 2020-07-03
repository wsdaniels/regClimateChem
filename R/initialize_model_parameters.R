initialize.model.parameters <- function(index.data, response.data, model.parameters){

  ###############################################################################
  # This function determines possible lag values for each index based on date
  # ranges in data and prestructures the model parameters structure in
  # preparation for user specification of most parameters.
  #
  # Input:
  # index.data a 1*n list. Required fields:
  #   index.data$year
  #   index.data$month
  #   index.data$name.of.index
  #   index.data$short.name
  #
  # response.data: A list. Required fields:
  #   response.data$name.of.region
  #   response.data$year
  #   response.data$month
  #   response.data$data.column.name
  #
  # model.parameters: A list.
  #
  # Output:
  # initialized.model.paramters: A copy of model.parameters, with populated fields:
  #
  #   initialized.model.paramters$name.of.region: Inherited from response.data
  #
  #   initialized.model.paramters$response.data.column.name: Inherited from response.data
  #
  #   initialized.model.paramters$response.data.time.window: A list with fields:
  #     response.data.time.window$start.year:  A 1*1 int, year response data begins
  #     response.data.time.window$start.month: A 1*1 int, month response data begins
  #     response.data.time.window$end.year:    A 1*1 int, year response data ends
  #     response.data.time.window$end.month:   A 1*1 int, month response data ends
  #
  #   initialized.model.paramters$index.data.time.window: A list with fields:
  #     index.data.time.window$start.year:    A 1*n int, year index data begins
  #     index.data.time.window$start.month:   A 1*n int, month index data begins
  #     index.data.time.window$end.year:      A 1*n int, year index data ends
  #     index.data.time.window$end.month:     A 1*n int, month index data ends
  #     index.data.time.window$name.of.index: A 1*n character vector, index names
  #
  #   initialized.model.paramters$lag.data.limits: A list with fields:
  #     lag.data.limits$name.of.index: A 1*n character vector, index names
  #     lag.data.limits$short.name:    A 1*n character vector, short index names
  #     lag.data.limits$max.data.lag:  A 1*n int, max possible lag for index given data
  #     lag.data.limits$min.data.lag:  A 1*n int, min possible lag for index given data
  ###############################################################################

  ## Duplicate existing struct contents
  initialized.model.parameters <- model.parameters

  ## Inherit region and response information
  initialized.model.parameters$name.of.region <- response.data$name.of.region
  initialized.model.parameters$response.data.column.name <- response.data$data.column.name

  ## Determine possible lag values
  # Harvest time extent of data
  response <- compute.date.extent(response.data$year, response.data$month)

  nIndices <- length(index.data[[3]])
  index <- matrix(ncol = length(response), nrow = nIndices)
  colnames(index) <- names(response)

  for (iIndex in 1:nIndices){
    index[iIndex,] <- unlist(compute.date.extent(index.data$year[iIndex],
                                                 index.data$month[iIndex]))
  }

  index <- as.data.frame(index)

  # Attach index names for ease of interpretability
  index <- cbind(index, index.data$name.of.index)
  colnames(index)[5] <- "name.of.index"


  # Determine lag range allowed by data for each index. Assumes full use of CO data is desired.
  lag <- data.frame(matrix(ncol = 4, nrow = nIndices))
  names(lag) <- c("name.of.index","short.name","max.data.lag","min.data.lag")

  for (iIndex in 1:nIndices){
    this.name.of.index <- index.data$name.of.index[iIndex]
    this.short.name <- index.data$short.name[iIndex]
    this.max.lag <- compute.date.lag(response$start.month,
                                     response$start.year,
                                     index$start.month[iIndex],
                                     index$start.year[iIndex])
    this.min.lag <- compute.date.lag(response$end.month,
                                     response$end.year,
                                     index$end.month[iIndex],
                                     index$end.year[iIndex])
    lag[iIndex,] <- c(this.name.of.index, this.short.name, this.max.lag, this.min.lag)
  }

  ## Attach time extents and lag limits to model parameter struct
  initialized.model.parameters$response.data.time.window <- response
  initialized.model.parameters$index.data.time.window <- index
  initialized.model.parameters$lag.data.limits <- lag

  ## Return initialized.model.parameters
  return(initialized.model.parameters)

}
