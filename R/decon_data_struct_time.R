decon.data.struct.time <- function(con.date.data){

  ###############################################################################
  # This function deconcatenates double time fields
  #
  # Input:
  # con.date.data: A 1*n list. Required fields:
  #   con.date.data$year.month.day: A m_i*1 double (m_i different for different
  #   indices) with dates stored as YYYMMDD
  #
  #   OR
  #
  #   con.date.data$year.month: A m_i*1 double with dates stored as YYYYMM.
  #
  # Output:
  # decon.date.data: A copy of con.date.data with additional separated "year"
  # and "month" fields.
  ###############################################################################

  ## Check fields
  required.field.one <- "year.month.day"
  has.field.one <- has.these.fields(con.date.data,required.field.one)

  required.field.two <- "year.month"
  has.field.two <- has.these.fields(con.date.data,required.field.two)

  if (has.field.one == F & has.field.two == F){
    print("Input list in decon.data.struct.time is missing a field.")
  }

  # Parse time field, dependent on structure (inferred from field name)
  decon.date.data <- con.date.data
  decon.date.data$year <- list()
  decon.date.data$month <- list()
  nIndices <- length(con.date.data[[2]])

  for (iIndex in 1:nIndices){
    if (has.field.one == T){
      this.year.month <- con.date.data$year.month.day[[iIndex]]
      time.data.list <- deconcatenate.year.month(this.year.month)
    }
    else if (has.field.two == T){
      this.year.month <- con.date.data$year.month[[iIndex]]
      time.data.list <- deconcatenate.year.month(this.year.month)
    }
    else{
      print("Warning: year.month.day / year.month fields are not present")
      time.data.list <- list(integer(), integer())
    }
    decon.date.data$year[[iIndex]] <- time.data.list[[1]]
    decon.date.data$month[[iIndex]] <- time.data.list[[2]]
  }

  ## Return decon.date.data
  return(decon.date.data)

}
