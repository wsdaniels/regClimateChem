rm(list = ls())

library(regClimateChem)
library(here)

dir.name <- "/home/wdaniels/Documents/research/co_project/technote/data/5_pred"

regions <- c("CentralSAfrica", "SouthSAfrica", "EastSAmerica", "WestSAmerica",
             "NorthAustralasia", "SouthAustralasia", "MaritimeSEA")

algorithms <- c("s")

num.trials <- c(1)

for (r in 1:length(regions)){

  for (a in 1:length(algorithms)){

    if (num.trials[a] != 0){

      for (t in 1:num.trials[a]){

        print(paste("Working on: ", regions[r], algorithms[a], as.character(t), sep = "   "))

        temp.output <- run.all(regions[r], algorithms[a])
        timing      <- temp.output[[1]]
        adj.r2.list <- temp.output[[2]]
        bic.list    <- temp.output[[3]]
        coef.list   <- temp.output[[4]]
        lagset.list <- temp.output[[5]]

        rm(temp.output)

        saveRDS(timing,      file = paste(dir.name, regions[r], algorithms[a], as.character(t),
                                          "timing.RData", sep = "/"))

        saveRDS(adj.r2.list, file = paste(dir.name, regions[r], algorithms[a], as.character(t),
                                          "adj_r2.RData", sep = "/"))

        saveRDS(bic.list,    file = paste(dir.name, regions[r], algorithms[a], as.character(t),
                                          "bic.RData",    sep = "/"))

        saveRDS(coef.list,   file = paste(dir.name, regions[r], algorithms[a], as.character(t),
                                          "coef.RData",   sep = "/"))

        saveRDS(lagset.list, file = paste(dir.name, regions[r], algorithms[a], as.character(t),
                                          "lagset.RData", sep = "/"))


      }
    }
  }
}
