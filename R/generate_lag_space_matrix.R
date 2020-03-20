generate.lag.space.matrix <- function(model.parameters){

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

  # Minimum number of lags to test for each index
  min.lags <- model.parameters$lag.user.limits$min.lag

  # Maximum number of lags to test for each index
  max.lags <- model.parameters$lag.user.limits$max.lag

  # Number of separately lagged terms for each index to include
  num.terms <- model.parameters$max.num.terms

  # Create list of possible lag combinations for each index
  combination.list <- list()

  for (i in 1:length(max.lags)){
    combination.list[[i]] <- combinations(n = max.lags[i] - min.lags[i] + 1,
                                          r = num.terms[i],
                                          v = min.lags[i]:max.lags[i])
  }

  # Create lagset matrix
  lagset.matrix <- matrix(nrow = 0, ncol = sum(num.terms))

  for (a in 1:nrow(combination.list[[1]])){
    for (b in 1:nrow(combination.list[[2]])){
      for (c in 1:nrow(combination.list[[3]])){
        for (d in 1:nrow(combination.list[[4]])){

          this.row <- cbind(t(combination.list[[1]][a,]),
                            t(combination.list[[2]][b,]),
                            t(combination.list[[3]][c,]),
                            t(combination.list[[4]][d,]))

          lagset.matrix <- rbind(lagset.matrix, this.row)

        }
      }
    }
  }

  return(lagset.matrix)

}
