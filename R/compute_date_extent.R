compute.date.extent <- function(this.year.list, this.month.list){
  
  ###############################################################################
  # This function returns the start and end dates in a list
  #
  # Input:
  # this.year.list: A vector of years, matched with:
  # this.month.list: A vector of months, assumed to be coherent with
  #   this.year.list and sorted ascending.
  #
  # Output:
  # date.extent: A list with four fields:
  #   date.extent$start.year: The first year in the list (sorted ascending)
  #   date.extent$start.month: The first month in the list
  #   date.extent$end.year: The last year in the list
  #   date.extent$end.month: The last month in the list
  ###############################################################################
  
  date.extent <- list()
  
  date.extent$start.year <- this.year.list[[1]][1]
  date.extent$start.month <- this.month.list[[1]][1]
  
  date.extent$end.year <- this.year.list[[1]][length(this.year.list[[1]])]
  date.extent$end.month <- this.month.list[[1]][length(this.month.list[[1]])]
  
  return(date.extent)

  }