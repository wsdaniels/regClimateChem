smooth <- function(index.data, window.size){

  ###############################################################################
  # This function smooths the index data.
  #
  # Input:
  #
  # index.data: a 1*n list.
  #
  # Output:
  #
  # index.data with smoothed anomaly data.
  ###############################################################################

  n.indices <- length(index.data$name.of.index)

  for (i in 1:n.indices){
    n.obs <- length(index.data$anomaly[[i]])

    for (j in 1:n.obs){

      if ( j <= window.size){
        index.data$anomaly[[i]][j] <- mean(index.data$anomaly[[i]][ 1:(j+window.size) ])

      } else if ( j > (n.obs-window.size)){
        index.data$anomaly[[i]][j] <- mean(index.data$anomaly[[i]][ (j-window.size):n.obs ])

      } else {
        index.data$anomaly[[i]][j] <- mean(index.data$anomaly[[i]][ (j-window.size):(j+window.size) ])

      }
    }
  }

  1+1

  return(index.data)


}
