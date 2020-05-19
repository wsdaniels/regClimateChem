rm(list = ls())

library(regClimateChem)
library(here)

dir.name <- "/home/wdaniels/Documents/research/co_project/technote/paper_data/5_pred"
# dir.name <- "/home/wdaniels/Documents/research/co_project/technote/data/4_pred"

regions <- c("CentralSAfrica", "CentralSAmerica", "MaritimeSEA",
             "NorthAustralasia", "SouthAustralasia", "SouthSAfrica", "SouthSAmerica")
# regions <- c("CentralSAfrica", "WestSAmerica", "MaritimeSEA",
             # "NorthAustralasia", "SouthAustralasia", "SouthSAfrica", "EastSAmerica")

algorithms <- c("h","g","s")
# algorithms <- c("g")

to.run <- c(2,7)

# for (r in 1:length(regions)){
for (r in to.run){

  for(a in 1:length(algorithms)){

    print(paste("Working on: ", regions[r], " ", algorithms[a], sep = ""))

    temp.output <- run.all(regions[r], algorithms[a])
    timing      <- temp.output[[1]]
    adj.r2.list <- temp.output[[2]]
    bic.list    <- temp.output[[3]]
    coef.list   <- temp.output[[4]]
    lagset.list <- temp.output[[5]]

    rm(temp.output)

    saveRDS(timing,      file = paste(dir.name, regions[r], algorithms[a], "timing.RData", sep = "/"))

    saveRDS(adj.r2.list, file = paste(dir.name, regions[r], algorithms[a], "adj_r2.RData", sep = "/"))

    saveRDS(bic.list,    file = paste(dir.name, regions[r], algorithms[a], "bic.RData",    sep = "/"))

    saveRDS(coef.list,   file = paste(dir.name, regions[r], algorithms[a], "coef.RData",   sep = "/"))

    saveRDS(lagset.list, file = paste(dir.name, regions[r], algorithms[a], "lagset.RData", sep = "/"))

  }

}
