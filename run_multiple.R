rm(list = ls())

library(regClimateChem)

### SETUP DIRECTORY STRUCTURE ###
# Set dir.name to the directory in which you would like to save the results
# A directory structure will be created automatically to hold the results
output.dir <- "/home/wdaniels/Desktop/test_dir"
dir.create(output.dir, showWarnings = FALSE)


### SETUP PARAMETER AND SEARCH SPACE ###
# List the regions you would like to study.
# By default all response regions are listed.
# NOTE: The region names must be the same as the names in the data .csv files
regions <- c("CentralSAfrica", "CentralSAmerica", "MaritimeSEA",
             "NorthAustralasia", "SouthAustralasia", "SouthSAfrica", "SouthSAmerica")

# List the parameter values you want to test
pop.size.vals <- c(100)
imm.rate.vals <- c(0.3)
sex.rate.vals <- c(0.1)
mut.rate.vals <- c(1e-3)
conseq.vals <- c(1,2,3,4,5)

test.vals <- conseq.vals

for (r in 1:length(regions)){

  for(a in 1:length(test.vals)){

    # Print progess report
    print(cat(paste0("Working on: ", regions[r], "\n",
                     "poulation size: ", pop.size.vals[1], "\n",
                     "immigration rate: ", imm.rate.vals[1], "\n",
                     "sexual reproduction rate: ", sex.rate.vals[1], "\n",
                     "mutation rate: ", mut.rate.vals[1], "\n",
                     "conseq value: ", conseq.vals[a], "\n",
                     "---------------------------------------------------")))

    # Run code with specific parameter settings
    this.output <- run.all(regions[r],
                           pop.size.vals[1],
                           imm.rate.vals[1],
                           sex.rate.vals[1],
                           mut.rate.vals[1],
                           conseq.vals[a])


    # Create filename
    this.pop.size <- sprintf('%.3e', pop.size.vals[1])
    this.imm.rate <- sprintf('%.3e', imm.rate.vals[1])
    this.sex.rate <- sprintf('%.3e', sex.rate.vals[1])
    this.mut.rate <- sprintf('%.3e', mut.rate.vals[1])
    this.conseq.val <- sprintf('%.3e', conseq.vals[a])

    file.name <- paste0(regions[r], "_",
                        this.pop.size, "_",
                        this.imm.rate, "_",
                        this.sex.rate, "_",
                        this.mut.rate, "_",
                        this.conseq.val, ".RData")

    # Save results as RData files
    saveRDS(this.output, file = paste(output.dir, file.name, sep = "/"))

  }
}
