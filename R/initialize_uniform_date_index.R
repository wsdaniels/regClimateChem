initialize.uniform.date.index <- function(response.data, index.data){

  ###############################################################################
  # This function reads the date series data in ResponseData, then computes and
  # attaches an offset value (computed as months before last response data
  # observation) to each datastream in ResponseData and IndexData for later use
  # in masking by lags.
  #
  # Input:
  # response.data: A list. Required fields:
  #   response.data$year
  #   response.data$month
  #
  # index.data: a 1*n list. Required fields:
  #   index.data$year
  #   index.data$month
  #
  # Output:
  # response.data.out: A copy of response.data with additional fields:
  #   response.data.out$last.response.month.offset
  #
  # index.data.out: A copy of index.data with additional fields:
  #   index.data.out$last.response.month.offset
  ###############################################################################

  # Assign initial values
  response.data.out <- response.data
  index.data.out <- index.data

  # Verify the presence of the necessary fields
  has.date.fields.response <- "year" %in% names(response.data) & "month" %in% names(response.data)
  has.date.fields.index <- "year" %in% names(index.data) & "month" %in% names(index.data)
  has.date.fields.both <- has.date.fields.response & has.date.fields.index

  if (has.date.fields.both == T){

    # Identify reference point
    terminal.response <- list()
    terminal.response$year <- response.data$year[[1]][length(response.data$year[[1]])]
    terminal.response$month <- response.data$month[[1]][length(response.data$month[[1]])]

    # Create offset vector for CO data
    nResponseObs <- length(response.data$year[[1]])
    response.offset.list <- vector()

    for (iResponseObs in 1:nResponseObs){
      this.month <- response.data$month[[1]][iResponseObs]
      this.year <- response.data$year[[1]][iResponseObs]
      this.offset <- as.integer(compute.date.lag(terminal.response$month, terminal.response$year,
                                      this.month, this.year))
      response.offset.list[iResponseObs] <- this.offset
    }

    # Attach
    response.data.out$last.response.month.offset <- response.offset.list

    # Create offset vector(s) for index data
    nIndices <- length(index.data$short.name)

    for (iIndex in 1:nIndices){
      this.nIndexObs <- length(index.data$year[[iIndex]])
      this.index.response.offset.list <- vector()

      for (jIndexObs in 1:this.nIndexObs){
        this.month <- index.data$month[[iIndex]][jIndexObs]
        this.year <- index.data$year[[iIndex]][jIndexObs]
        this.offset <- as.integer(compute.date.lag(terminal.response$month, terminal.response$year,
                                        this.month, this.year))
        this.index.response.offset.list[jIndexObs] <- this.offset
      }
      # Attach
      index.data.out$last.response.month.offset[[iIndex]] <- this.index.response.offset.list
    }
  }
  else{
    print("Data fields not found in input structs. No changes made.")
  }

  temp.output <- list("response" = response.data.out, "index" = index.data.out)

}
