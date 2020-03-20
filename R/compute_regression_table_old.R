compute.regression.table.old <- function(response.data, index.data, model.parameters, this.lag.set){

  ###############################################################################
  # This function creates a table used in the stepwise regression using data
  # and parameters.
  #
  # Input:
  # response.data: A list. Required fields:
  #   response.data$response.var
  #   response.data$month
  #   response.data$last.response.month.offset
  #
  # index.data: A 1*n list. Required fields:
  #   index.data$anomaly[i]
  #   index.data$month[i]
  #   index.data$last.response.month.offset[i]
  #
  # model.parameters: A list. Required fields:
  #   model.parameters$output.period.mask
  #
  # this.lag.set: A 1*n int vector, with lags for each of the n indices.
  #
  # Output:
  # regression.table: A dataframe assembled from response data and index data
  #   over the window of interest specified in model.parameters, with
  #   with appropriate lags applied (as specified in this.lag.set).
  ###############################################################################

  browser()

  # verify that this.lag.set is the correct size
  nIndices <- length(index.data$short.name)
  is.lag.set.correct.size <- length(this.lag.set) == nIndices

  if (is.lag.set.correct.size == F){
    print("Number of indices does not match the size of the lag set")
  }

  these.observations <- response.data$response.var[[1]]
  these.months <- response.data$month[[1]]
  these.offsets <- response.data$last.response.month.offset

  # Reduce response data. Mask in only the months we care about.
  this.mask <- model.parameters$output.period.mask
  location.in.these.months <- is.element(these.months, this.mask)

  # Attach to a temp struct
  masked.response.data <- list()
  masked.response.data$response <- these.observations[location.in.these.months]
  masked.response.data$month <- these.months[location.in.these.months]
  masked.response.data$offset <- these.offsets[location.in.these.months]
  masked.response.data$short.name <- "Response"

  # Reduce response data. Remove entries without corresponding index data.
  for (iIndex in 1:nIndices){

    # Get index offsets
    these.offsets <- index.data$last.response.month.offset[[iIndex]]

    # Find the response-referenced timestamps with correct lags
    this.lagged.offset.mask <- masked.response.data$offset + this.lag.set[iIndex]

    # Identify desired offsets missing in index data
    which.lagged.offsets.are.present <- is.element(this.lagged.offset.mask, these.offsets)

    # Slice out the rows without corresponding data from struct
    masked.response.data$response <- masked.response.data$response[which.lagged.offsets.are.present]
    masked.response.data$month    <- masked.response.data$month[which.lagged.offsets.are.present]
    masked.response.data$offset   <- masked.response.data$offset[which.lagged.offsets.are.present]

  }

  # Slice index data at specific lags
  masked.lagged.index.data <- list()

  for (iIndex in 1:nIndices){

    # Get full data extent
    these.observations <- index.data$anomaly[[iIndex]]
    these.months <- index.data$month[[iIndex]]
    these.offsets <- index.data$last.response.month.offset[[iIndex]]

    # Find the response-referenced timestamps with correct lags
    this.lagged.offset.mask <- masked.response.data$offset + this.lag.set[[iIndex]]
    this.index.offset.list <- index.data$last.response.month.offset[[iIndex]]
    location.in.these.offsets <- is.element(this.index.offset.list, this.lagged.offset.mask)

    # Slice and attach to temp struct
    masked.lagged.index.data$anomaly[[iIndex]] <- these.observations[location.in.these.offsets]
    masked.lagged.index.data$month[[iIndex]] <- these.months[location.in.these.offsets]
    masked.lagged.index.data$offset[[iIndex]] <- these.offsets[location.in.these.offsets]
    masked.lagged.index.data$short.name[[iIndex]] <- index.data$short.name[[iIndex]]

  }

  # Build a table and header from temp structs
  tab <- data.frame(masked.response.data$response)

  for (iIndex in 1:nIndices){
    this.column <- masked.lagged.index.data$anomaly[[iIndex]]
    tab <- cbind(tab, this.column)

  }

  names(tab) <- c(masked.response.data$short.name, index.data$short.name)

 return(tab)

}
