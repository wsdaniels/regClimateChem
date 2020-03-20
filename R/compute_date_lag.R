compute.date.lag <- function(ref.month, ref.year, check.month, check.year){
  
  ###############################################################################
  # This function computes the lag (in months) between two dates.
  #
  # Input:
  # ref.month, ref.year, check.month, check.year: All 1*1 numerics
  #
  # Output:
  # lag.in.months: An int, the lag in months by which the check date precedes
  #   reference date. F,or example: 
  #   compute.date.lag(8,20127,2011) returns 13, indicating that July 2011
  #   occurs 13 months before August 2012.
  #
  # Note: if the check date occurs after the reference date, the returned lag
  #       will be negative.
  ###############################################################################
  
  lag.in.months <- 12 * (ref.year - check.year) + (ref.month - check.month)
  
  return(lag.in.months)
  
}