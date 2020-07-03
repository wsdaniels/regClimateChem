regress.over.lagset <- function(lag.space.matrix, response.data, index.data, model.parameters, conseq.val){

  ###############################################################################
  # This function computes a goodness of fit statistic (adjusted r squared) for
  # each lag set in the lag.space.matrix.
  #
  # Input:
  # lag.space.matrix: An m*n matrix, where lag.space.matrix[i,] represents a
  #   particular lagset over the n indices to be investigated.
  #
  # reseponse.data: A list. Required fields:
  #   response.data$response.var
  #   response.data$month
  #   response.data$last.response.month.offset
  #
  # index.data: a 1*n list. Required fields:
  #   index.data$anomaly[i]
  #   index.data$month[i]
  #   index.data$last.response.month.offset[i]
  #
  # model.parameters: A list. Required fields:
  #   model.parameters$output.period.mask
  #   model.parameters$should.search.parallel
  #   model.parameters$regression.selection.upper.model
  #
  # Output:
  # goodness.of.fit.list: An m*1 double containing the goodness of fit statistic
  #   computed from the model object of the terminal model attained during the
  #   stepwise selection for each lagset examined.
  ###############################################################################

  # Initialize variables
  # nLagSets <- nrow(lag.space.matrix)
  nLagSets <- 10
  goodness.of.fit.list <- list()

  # Initialize parallization parameters
  cl <- makeCluster(model.parameters$number.of.cores.to.use)
  registerDoParallel(cl)

  # Loop over each lagset
  goodness.of.fit.list <- foreach(iLagSet = 1:nLagSets, .combine = 'c') %dopar%{
  # for (iLagSet in 1:nLagSets){

    # Add glmulti library
    library(glmulti)

    # Get lagset out of lagset matrix
    this.lag.set <- lag.space.matrix[iLagSet,]

    # Compute regression table
    this.regression.table <- compute.regression.table(response.data, index.data,
                                                      model.parameters, this.lag.set)

    if (model.parameters$search.algorithm != "s"){
      # Perform the exhaustive or genetic regression
      dummy <- capture.output(
        obj <- glmulti(y = "Response",
                       xr = names(this.regression.table)[2:length(names(this.regression.table))],
                       data = this.regression.table,
                       marginality = T,
                       level = 2,
                       method = model.parameters$search.algorithm,
                       crit = model.parameters$regression.selection.criterion,
                       plotty = F,
                       popsize = 40,
                       confsetsize = 1,
                       conseq = 6,
                       imm = 0.35)
      )

      best.model <- obj@objects[[1]]

    } else {

      # Perform stepwise selection
      library(MASS)
      dummy <- capture.output(
        full.model <- lm(Response ~ .*., data = this.regression.table))
      dummy <- capture.output(
        best.model <- stepAIC(full.model, direction = "both", k = log(nrow(this.regression.table))))

    }

    # Calculate BIC
    ll <- logLik(best.model)
    ll.val <- ll[1]
    num.params <- attr(ll, "df") - 1
    num.obs <- length(best.model$residuals)
    bic <- log(num.obs) * num.params - 2 * ll.val

    # Calculate adj r2
    if (model.parameters$search.algorithm != "s"){
      r.squared <- 1 - (best.model$deviance / best.model$null.deviance)
      sample.size <- length(best.model$residuals)
      num.predictors <- length(best.model$coefficients) - 1
      adj.r.squared <- 1 - (1 - r.squared)*((sample.size - 1)/(sample.size - num.predictors - 1))
    } else {
      adj.r.squared <- summary(best.model)$adj.r.squared
    }


    # Save coefficients for best model
    model.coef <- best.model$coefficients

    # Combine r2 and coef names into list to save
    to.save <- list(adj.r.squared, bic, model.coef, this.lag.set)

  }

  stopCluster(cl)

  return(goodness.of.fit.list)

}
