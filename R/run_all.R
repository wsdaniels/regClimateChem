run.all <- function(region, algorithm){

  ############################################################################################
  # Demonstration script for high-level structures and functions associated with
  # the Buchholz release code project

  # Created by Will Daniels (wdaniels@mymail.mines.edu)
  # Based on MATLAB code written by Peter Simonson (petersimonson@mines.edu)
  # Version 17-September-2019
  ############################################################################################

  #### WORKSPACE CLEANUP ####
  #rm(list = ls()) # clear environment
  #cat("\014") # Clear console

  ### IMPORT LIBRARIES ####
  library(foreach)
  library(doParallel)
  library(regClimateChem)
  library(gtools)

  #### PATH SETTING ####
  # The following must be included in the working directory:
  #   A subdirectory that contains the response and index data
  # setwd("/home/jake/Desktop/math530/co_project/scripts")
  setwd("/home/wdaniels/Documents/research/co_project/regClimateChem")


  #### LOAD USER SPECIFICATIONS ####
  # The user config script is where the user sets information about how to run the linear
  # regression and lag search.
  # example_user_config_script is provided as an example of necessary configuration elements.
  #source("main_code_base/example_user_config_script.R")
  temp.output <- config.func(region, algorithm)
  index.data       <- temp.output[[1]]
  model.parameters <- temp.output[[2]]
  response.data    <- temp.output[[3]]
  rm(temp.output)


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
  #debug(regress.over.lagset)

  model.evaluation.metric.list <- regress.over.lagset(lag.space.matrix, response.data,
                                                      index.data.smoothed, model.parameters,
                                                      conseq.val)

  adj.r2.list <- unlist(model.evaluation.metric.list[seq(1,length(model.evaluation.metric.list),4)])
  bic.list <- unlist(model.evaluation.metric.list[seq(2,length(model.evaluation.metric.list),4)])
  coef.list <- model.evaluation.metric.list[seq(3,length(model.evaluation.metric.list),4)]
  lagset.list <- model.evaluation.metric.list[seq(4,length(model.evaluation.metric.list),4)]

  # bic.list <- unlist(model.evaluation.metric.list[seq(1,length(model.evaluation.metric.list),3)])
  # coef.list <- model.evaluation.metric.list[seq(2,length(model.evaluation.metric.list),3)]
  # lagset.list <- model.evaluation.metric.list[seq(3,length(model.evaluation.metric.list),3)]

  rm(model.evaluation.metric.list)


  end.time <- Sys.time()
  total.time <- difftime(end.time, start.time)[[1]]


  to.return <- list(list(start.time,end.time,total.time),
                    adj.r2.list, bic.list, coef.list, lagset.list)

  return(to.return)

}
