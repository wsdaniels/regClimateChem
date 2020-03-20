construct.month.mask <- function(model.parameters){
  
  ###############################################################################
  # This function builds a mask for months of interest given endpoints
  #
  # Input:
  # model.parameters: A list. Required fields:
  #   model.parameters$output.period.start
  #   model.parameters$output.period.end
  #
  # Output:
  # month.mask: A 1*n list of the months that are within the output period.
  ###############################################################################
  
  this.start <- model.parameters$output.period.start
  this.end <- model.parameters$output.period.end
  
  if (this.start <= this.end){
    month.mask <- this.start : this.end
  }
  else{
    month.mask <- c(this.start : 12, 1 : this.end)
  }
  
  return(month.mask)
  
}