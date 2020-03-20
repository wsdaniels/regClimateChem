deconcatenate.year.month <- function(year.month){
  
  ###############################################################################
  # This function parses YYYYMM double into two integers.
  #
  # Input:
  # year.month: A double in either YYYYMM or YYYYMMDD format. 
  # 
  # Output:
  # time.data.list: A list containing two integer vectors, year and month. 
  ###############################################################################
 
  year <- as.integer(substr(year.month,1,4))
  month <- as.integer(substr(year.month,5,6))
    
  time.data.list <- list("year" = year, "month" = month)  
 
  
  return(time.data.list)
}