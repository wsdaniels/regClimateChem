complete.model.parameters <- function(model.parameters, index.data){

  ###############################################################################
  # This function completes the model.parameters object after user input.
  #
  # Input:
  # model.parameters: A list. Required fields:
  #   model.parameters$output.period.start
  #   model.parameters$output.period.end
  #
  # index.data: a 1*n list.
  #
  # Output:
  # complete.model.parameters: A copy of model.parameters with fields:
  #   output.period.start: An int
  #   output.period.end: An int
  #   output.period.mask: A 1*m int of months to look at for the response
  #     variable
  #   regression.string: A 1*p character vector with a regression formula used
  #     as the base of the stepwise linear regression.
  ###############################################################################


  # Prepopulate
  completed.model.parameters <- model.parameters

  # Generate the month mask from the limits
  completed.model.parameters$output.period.mask <- construct.month.mask(model.parameters)

  # Generate the regression model string from the short names
  completed.model.parameters$regression.string <- initialize.regression.string(index.data)

  return(completed.model.parameters)

}
