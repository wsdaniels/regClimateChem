############################################################################################
# Demonstration script for high-level structures and functions associated with
# the Buchholz release code project

# Created by Will Daniels (wdaniels@mymail.mines.edu)
# Based on MATLAB code written by Peter Simonson (petersimonson@mines.edu)
# Version 17-September-2019
############################################################################################

#### WORKSPACE CLEANUP ####
rm(list = ls()) # clear environment
cat("\014") # Clear console

### IMPORT LIBRARIES ####
library(foreach)
library(doParallel)
library(regClimateChem)
library(gtools)
library(here)

#### PATH SETTING ####
# Set the working directory to the directory in which this file is saved.
# The following must be included in the working directory:
#   A subdirectory that contains the response and index data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Set the output directory to the directory in which the output is saved.
# The output of the run_once.R script is a single .RData file
output.dir <- "/home/wdaniels/Desktop/test_dir"
dir.create(output.dir, showWarnings = FALSE)



#### LOAD USER SPECIFICATIONS ####
# The user config script is where the user sets information about how to run the linear
# regression and lag search.
source("single_run_config_file.R")


#### TIMING ####
start.time <- Sys.time()


#### DATA PROCESSING: RESPONSE DATA IMPORT ####
# Read in the CO data from the specified location and reformat the date for later use.
response.data <- populate.response.data(response.data)
response.data <- decon.data.struct.time(response.data)


#### DATA PROCESSING: EXPLANATORY DATA IMPORT ####
# Read the Index data from the specified locations and reformat the date.
# Note: dates are assumed to be in different formats between files.
# Specifically, YYYYMM format in the CO data and YYYYMMDD in the index data.
index.data <- populate.index.data(index.data)
index.data <- decon.data.struct.time(index.data)


#### DATA PROCESSING: INITIALIZE MODEL PARAMETERS STRUCTURE ####
# Determines the temporal extent of each data stream, and computes the lag space that the
# data will support (assuming full utilization of the CO data).
model.parameters <- initialize.model.parameters(index.data, response.data, model.parameters)


#### DATA PROCESSING: MODEL PARAMETER STRCUTRE COMPLETION ####
# Computes and attaches elements needed for data slicing and regression: masks and
# regression formula.
model.parameters <- complete.model.parameters(model.parameters, index.data)


#### DATA PROCESSING: CONSTRUCT AND ATTACH DATE OFFSET ####
# Applies date offset, referenced to the last month of response data present in the stream.
temp.output <- initialize.uniform.date.index(response.data, index.data)
response.data <- temp.output$response
index.data <- temp.output$index
rm(temp.output)


#### DATA PROCESSING: CONSTRUCT A LAG SET MATRIX ####
# Generate the full Cartesian product sets of potential lags for the model search,
# based on the user-specified lag limits.
lag.space.matrix <- generate.lag.space.matrix(model.parameters)


#### DATA PROCESSING: SMOOTH INDEX DATA ####
# Smooth index data
index.data.smoothed <- smooth(index.data, model.parameters$smoothing.parameter)


#### DATA PROCESSING: LAG SPACE SEARCH ####
# Stepwise regression for every lag set that appears in the lagSpaceMatrix.
# For each lag set, only a model evaluation metric for the final model produced by the
# selection process is preserved.
model.evaluation.metric.list <- regress.over.lagset(lag.space.matrix, response.data,
                                                    index.data.smoothed, model.parameters)


### DATA PACKAGING ###
# Unlist output from regress.over.lagset
adj.r2.list <- unlist(model.evaluation.metric.list[seq(1,length(model.evaluation.metric.list),4)])
bic.list    <- unlist(model.evaluation.metric.list[seq(2,length(model.evaluation.metric.list),4)])
coef.list   <-        model.evaluation.metric.list[seq(3,length(model.evaluation.metric.list),4)]
lagset.list <-        model.evaluation.metric.list[seq(4,length(model.evaluation.metric.list),4)]

# Finish timer
end.time <- Sys.time()
total.time <- difftime(end.time, start.time, units = "secs")[[1]]

# Put all relavant information about the particular run into an output object
output.list <- list(adj.r2.list = adj.r2.list,
                    bic.list = bic.list,
                    coef.list = coef.list,
                    lagset.list = lagset.list,
                    run.time = total.time,
                    run.parameters = list(model.parameters = model.parameters,
                                          index.data = index.data,
                                          response.data = response.data))

# Cleanup
rm(model.evaluation.metric.list, adj.r2.list, bic.list, coef.list, lagset.list)
gc()

### SAVE OUTPUT ###
# Save output object to the output directory
saveRDS(output.list, paste(output.dir, "single_run_output.RData", sep = "/"))

