generate.lag.space.matrix.old <- function(model.parameters){

  ###############################################################################
  # This function returns a matrix of every set of lag combinations for the
  # indices used and lags desired.
  #
  # Input:
  # model.parameters: A list. Required fields:
  #   model.parameters$lag.user.limits
  #
  # Output:
  # lag.space.matrix: An m*n data frame, where m is the total number of lag
  #   combinations possible, and n is the total number of indices used.
  #   Entry [i,j] is therefore the lag associated with the index j and the lag
  #   set i.
  ###############################################################################

  nIndices <- length(model.parameters$lag.user.limits[[1]])
  lag.list.cell.array <- list()

  for (iIndex in 1:nIndices){
    this.min.lag <- model.parameters$lag.user.limits$min.lag[iIndex]
    this.max.lag <- model.parameters$lag.user.limits$max.lag[iIndex]
    this.lag.list <- this.min.lag : this.max.lag
    lag.list.cell.array[[iIndex]] <- this.lag.list
  }

  lag.space.matrix <- as.matrix(expand.grid(lag.list.cell.array))

  return(lag.space.matrix)

}
