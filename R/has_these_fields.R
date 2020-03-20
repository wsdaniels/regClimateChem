has.these.fields <- function(struct, field.name.array){
  
  ###############################################################################
  # This function checks if input struct has all fields listed in 
  # field.name.array.
  #
  # Input:
  # struct: A list.
  # field.name.array: A 1*n string vector. Each element is checked as a field in 
  #   struct.
  # 
  # Output:
  # has.all.fields: Logical true if all names in the field.name.array are fields
  #   in struct. Logical false otherwise.
  ###############################################################################  
  
  nNames <- length(field.name.array)
  in.struct.list <- rep(F,nNames)
  
  
  ## Roll through  the string vector and check each for presence in struct
  for (iName in 1:nNames){
    this.name.string <- field.name.array[iName]
    these.struct.names <- names(struct)
    this.name.in.struct <- this.name.string %in% these.struct.names
    in.struct.list[iName] <- this.name.in.struct
  }
 
  ## Aggregate results
  has.all.fields <- all(in.struct.list)
  return(has.all.fields)
  
}