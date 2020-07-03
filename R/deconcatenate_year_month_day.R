deconcatenate.year.month.day <- function(year.month.day){

  ###############################################################################
  # This function parses YYYYMM double into two integers.
  #
  # Input:
  # year.month: A double in either YYYYMM or YYYYMMDD format.
  #
  # Output:
  # time.data.list: A list containing two integer vectors, year and month.
  ###############################################################################

  year  <- as.integer(substr(year.month.day, 1, 4))
  month <- as.integer(substr(year.month.day, 5, 6))
  day   <- as.integer(substr(year.month.day, 7, 8))

  time.data.list <- list("year" = year, "month" = month, "day" = day)


  return(time.data.list)
}
