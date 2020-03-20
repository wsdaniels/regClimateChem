compute.regression.table <- function(response.data, index.data, model.parameters, this.lag.set){

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

  # Get number of indices and predictors
  num.indices <- length(model.parameters$lag.data.limits$short.name)
  total.num.pred <- sum(model.parameters$max.num.terms)

  # Get response data
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

  lag.counter <- 1L

  # Reduce response data. Remove entries without corresponding index data.
  for (i in 1:num.indices){

    num.pred <- model.parameters$max.num.terms[i]

    for (j in 1:num.pred){

      # Get index offsets
      these.offsets <- index.data$last.response.month.offset[[i]]

      # Find the response-referenced timestamps with correct lags
      this.lagged.offset.mask <- masked.response.data$offset + this.lag.set[lag.counter]
      lag.counter <- lag.counter + 1L

      # Identify desired offsets missing in index data
      which.lagged.offsets.are.present <- is.element(this.lagged.offset.mask, these.offsets)

      # Slice out the rows without corresponding data from struct
      masked.response.data$response <- masked.response.data$response[which.lagged.offsets.are.present]
      masked.response.data$month    <- masked.response.data$month[which.lagged.offsets.are.present]
      masked.response.data$offset   <- masked.response.data$offset[which.lagged.offsets.are.present]

    }
  }

  # Slice index data at specific lags
  masked.lagged.index.data <- list()
  lag.counter <- 1L

  for (i in 1:num.indices){

    num.pred <- model.parameters$max.num.terms[i]

    for (j in 1:num.pred){

      # Get full data extent
      these.observations <- index.data$anomaly[[i]]
      these.months       <- index.data$month[[i]]
      these.offsets      <- index.data$last.response.month.offset[[i]]

      # Find the response-referenced timestamps with correct lags
      this.lagged.offset.mask   <- masked.response.data$offset + this.lag.set[lag.counter]
      location.in.these.offsets <- is.element(these.offsets, this.lagged.offset.mask)

      # Slice and attach to temp struct
      masked.lagged.index.data$anomaly[[lag.counter]]    <- these.observations[location.in.these.offsets]
      masked.lagged.index.data$month[[lag.counter]]      <- these.months[location.in.these.offsets]
      masked.lagged.index.data$offset[[lag.counter]]     <- these.offsets[location.in.these.offsets]
      masked.lagged.index.data$short.name[[lag.counter]] <- paste(index.data$short.name[[i]],j, sep = "_")

      lag.counter <- lag.counter + 1L
    }
  }

  # Build a table and header from temp structs
  tab <- data.frame(masked.response.data$response)

  for (i in 1:total.num.pred){
    this.column <- masked.lagged.index.data$anomaly[[i]]
    tab <- cbind(tab, this.column)

  }

  names(tab) <- c(masked.response.data$short.name, masked.lagged.index.data$short.name)

  return(tab)

}
