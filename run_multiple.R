rm(list = ls())

library(regClimateChem)

### SETUP DIRECTORY STRUCTURE ###
# Set dir.name to the directory in which you would like to save the results
# A directory structure will be created automatically to hold the results
dir.name <- "/home/wdaniels/Desktop/test_dir"
dir.create(dir.name, showWarnings = FALSE)


### SETUP PARAMETER AND SEARCH SPACE ###
# List the regions you would like to study.
# By default all response regions are listed.
# NOTE: The region names must be the same as the names in the data .csv files
regions <- c("CentralSAfrica", "CentralSAmerica", "MaritimeSEA",
             "NorthAustralasia", "SouthAustralasia", "SouthSAfrica", "SouthSAmerica")

# List the algorithsm you would like to test
algorithms <- c("s")

for (r in 1:length(regions)){

  for(a in 1:length(algorithms)){

    # Print progess report
    print(paste("Working on: ", regions[r], " ", algorithms[a], sep = ""))

    # Run code with specific parameter settings
    temp.output <- run.all(regions[r], algorithms[a])
    timing      <- temp.output[[1]]
    adj.r2.list <- temp.output[[2]]
    bic.list    <- temp.output[[3]]
    coef.list   <- temp.output[[4]]
    lagset.list <- temp.output[[5]]

    rm(temp.output)

    # Create specific directory to hold results
    dir.create(paste(dir.name, regions[r], sep = "/"), showWarnings = FALSE)
    this.dir <- paste(dir.name, regions[r], algorithms[a], sep = "/")
    dir.create(this.dir, showWarnings = FALSE)

    # Save results as RData files
    saveRDS(timing,      file = paste(this.dir, "timing.RData", sep = "/"))
    saveRDS(adj.r2.list, file = paste(this.dir, "adj_r2.RData", sep = "/"))
    saveRDS(bic.list,    file = paste(this.dir, "bic.RData",    sep = "/"))
    saveRDS(coef.list,   file = paste(this.dir, "coef.RData",   sep = "/"))
    saveRDS(lagset.list, file = paste(this.dir, "lagset.RData", sep = "/"))

  }
}
