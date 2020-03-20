initialize.regression.string <- function(index.data){
  
  ###############################################################################
  # This function creates the string to be used in the regression function 
  # using the index short names.
  #
  # Input:
  # index.data: A 1*n list. Required fields:
  #   index.data$short.name: A character vector
  #
  # Output:
  # regression.string: A 1*m character vector structured to serve as a regression
  #   formula argument in the stepwise regression function. It is based on the 
  #   index short names.
  ###############################################################################
  
  # Infer size
  nIndices <- length(index.data$short.name)
  
  # Verify field has character arrays
  if (is.character(index.data$short.name == F)){
    print("In initialize.regression.string, not all index.data$short.name are characters")
  }
  
  # Build character array regression string
  regression.string = "Response ~ "
  for (iIndex in 1:nIndices){
    regression.string = c(regression.string, index.data$short.name[iIndex])
    if (iIndex < nIndices){
      regression.string = c(regression.string, " + ")
    }
  }
  
  regression.string <- paste(regression.string, sep = "", collapse = "")
  
  return(regression.string)
}