config.func <- function(region, algorithm){

  #### WORKSPACE AND STRUCTURE GUIDE ####
  # This codebase is built on three basic data structures:
  # index.data (1*n), where climate mode index data for n indices are stored.
  # response.data (1*1), where response data are stored.
  # model.parameters (1*1), where modeling and processing information is stored.
  #
  # Each of these three structs contain fields that must be specified by the
  # user. This script initializes all three of these structs, and provides
  # example field values required to reproduce the example analysis found in
  # Buchholz et al. (2018).

  # Users are encouraged to modify the settings in this script and observe the
  # changes that take place when the demonstration script is run thereafter.

  #### USER INPUT: RESPONSE REGION AND DATA SOURCE ####
  # Initialize response data struct and define region name, data location,
  # and which column in target file contains response data.
  response.data <- list()

  response.data$name.of.region <- region
  response.data$file.path <- "data/Carbon_Monoxide_Data"
  response.data$file.name <- paste(region, "_V7TMOPITTanomalies.csv", sep = "")
  response.data$file.data.column.index <- 3L


  #### USER INPUT: CLIMATE MODE INDEX DATA SOURCES ####
  # Initialize Index data struct and define Index names and data locations.
  # Users may specify as many or as few indices as desired.
  # The functions are written for a generalized index count.
  index.data <- list()

  index.data$name.of.index = c("Southern Annular Mode (AAO)",   # INDEX 1
                               "Dipole Mode Index (IOD)",       # INDEX 2
                               "Nino 3.4 (ENSO)",               # INDEX 3
                               "Tropical South Atlantic (TSA)") # INDEX 4

  index.data$short.name = c("AAO",  # INDEX 1
                            "IOD",  # INDEX 2
                            "ENSO", # INDEX 3
                            "TSA")  # INDEX 4

  index.data$file.path = c("data/Climate_Mode_Index_Data",  # INDEX 1
                           "data/Climate_Mode_Index_Data",  # INDEX 2
                           "data/Climate_Mode_Index_Data",  # INDEX 3
                           "data/Climate_Mode_Index_Data")  # INDEX 4

  index.data$file.name = c("aao_month_avg.csv",    # INDEX 1
                           "dmi_2_month_avg.csv",  # INDEX 2
                           "nino34_month_avg.csv", # INDEX 3
                           "satl_month_avg.csv")   # INDEX 4


  #### USER INPUT: PERIOD OF INTEREST ####
  # output.period.start and output.period.end define the period over which we want to
  # explain response data values. For example, to consider the October through
  # January window, set output.period.start <- 10, output.period.end <- 1.
  # Response data that are outside of this window are not utilized in the analysis.
  model.parameters <- list()

  model.parameters$output.period.start = 9L
  model.parameters$output.period.end = 12L


  #### USER INPUT: MODEL SELECTION EVALUATION CRITERION ####
  # regression.selection.criterion sets the evaluation criterion used for model
  # selection within each particular lagset:
  # 'aic' | 'bic'
  model.parameters$regression.selection.criterion = "bic"


  #### USER INPUT: SEARCH ALGORITHM ####
  # sets the search algorithm.
  # can be either exhaustive ("h") or genetic ("g")
  model.parameters$search.algorithm = algorithm


  #### USER INPUT: OPTIMIZATION CRITERION ####
  # set the criterion used to select the best model between all lagsets
  # Can be either bic ("bic") or adjusted r squared ("adjr2")
  model.parameters$optimization.criterion = "bic"


  #### USER INPUT: LAGSET GRID SEARCH RANGES ####
  # lag.user.limits defines the lags to explore for each Index.
  # lag.user.limits$min.lag[i] sets the minimum lag to consider for index i.
  # lag.user.limits$max.lag[i] sets the maximum lag to consider for index i.
  # Index ordering must match index.data struct ordering.
  # Examine index.data$name.of.index for indices used and their order.
  model.parameters$lag.user.limits = list( "min.lag" = c(1,  # INDEX 1
                                                         1,  # INDEX 2
                                                         1,  # INDEX 3
                                                         1), # INDEX 4

                                           "max.lag" = c(8,  # INDEX 1
                                                         8,  # INDEX 2
                                                         8,  # INDEX 3
                                                         8)) # INDEX 4

  #### USER INPUT: LAGSET GRID SEARCH IN SERIAL / PARALLEL ####
  # should.search.parallel sets whether the search for best models will be
  # completed using parallel computing. If false, or if the R parallel
  # computing package is not available, the search occurs serially.
  model.parameters$should.search.parallel <- T

  # number.of.cores.to.use sets the number of parallel nodes to create. By
  # default, it is set to the number of CPUs available minus one (so that
  # the machine is still usable).
  model.parameters$number.of.cores.to.use <- detectCores()-1

  #### USER INPUT: SMOOTHING PARAMETER ####
  # Sets width of smoothing window
  # If set to 0, no smoothing will be performed.
  # If set to i, each data point will be averaged with its 2*i closest
  # neighbors (on both sides of the data point)
  model.parameters$smoothing.parameter <- 0

  #### USER INPUT: MAX NUMBER OF LAGS PER INDEX ####
  # Indicate the maximum number of lagged terms per index that can be
  # included in the model
  model.parameters$max.num.terms = c(1,  # INDEX 1
                                     1,  # INDEX 2
                                     2,  # INDEX 3
                                     1)  # INDEX 4

  to.return <- list(index.data, model.parameters, response.data)

  return(to.return)

}
