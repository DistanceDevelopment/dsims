#' Method to run a simulation
#'
#' Runs the simulation and returns the simulation object with results. If
#' running in parallel and max.cores is not specified it will default to using
#' one less than the number of cores / threads on your machine. For example
#' code see \code{\link{make.simulation}}
#'
#' @param simulation \code{\link{Simulation-class}} object
#' @param run.parallel logical option to use multiple processors
#' @param max.cores integer maximum number of cores to use, if not specified then
#' one less than the number available will be used.
#' @param counter logical indicates if you would like to see the progress counter.
#' @param ... will allow further options to be implemented
#' @return the \code{\link{Simulation-class}} object which now includes
#' the results
#' @export
#' @importFrom parallel detectCores makeCluster clusterEvalQ stopCluster parLapply
#' @importFrom rstudioapi versionInfo
#' @rdname run.simulation-methods
#' @seealso \code{\link{make.simulation}}
run.simulation <- function(simulation, run.parallel = FALSE, max.cores = NA, counter = TRUE, ...){
  save.data <- load.data <- FALSE
  data.path <- character()
  #Process ... arguments
  args <- list(...)
  transect.path <- character(0)
  if("transect.path" %in% names(args)){
    transect.path <- args$transect.path
  }
  #Check if it is a single transect set or a folder
  if(length(transect.path) > 0){
    stop("Uploading transects from file is not yet implemented.", call. = FALSE)
    check.path <- unlist(strsplit(transect.path, split = "[.]"))
    index <- length(check.path)
    if(check.path[index] == "shp"){
      message(paste("run.simulation has detected that you wish to use a single set of transects for the simulation. Using shapefile: ", transect.path, sep = ""))
    }else{
      message(paste("run.simulation has detected that you wish to use pre-existing shapefile. These will be loaded from the following directory: ", transect.path, sep = ""))
    }
  }else{
    single.transect = FALSE
  }
  #Reset results arrays
  simulation@results <- create.results.arrays(simulation@reps,
                                              simulation@design@region,
                                              simulation@ds.analysis,
                                              simulation@population.description)
  #reset the error/warning message
  simulation@warnings$message <- list()
  simulation@warnings$counter <- list()
  #check the data.path ends in "/"
  if(length(data.path) > 0){
    temp.path <- strsplit(data.path, split = "")
    if(temp.path[length(temp.path)] != "/"){
      # if not add it
      data.path <- paste(data.path, "/", sep = "")
    }
    rm(temp.path)
  }
  # Two if(run.parallel) checks as if libraries not present will change to
  # false before running in parallel and then run in serial
  if(run.parallel){
    if(!requireNamespace('parallel', quietly = TRUE) | !requireNamespace('pbapply', quietly = TRUE)){
      warning("Could not run in parallel, library(pbapply) or library(parallel) is not installed.", immediate. = TRUE, call. = FALSE)
      run.parallel = FALSE
    }else{
      # counts the number of cores you have
      nCores <- getOption("cl.cores", detectCores()) - 1
      if(!is.na(max.cores)){
        nCores <- min(nCores, max.cores)
      }
      if(nCores <= 1){
        warning("Could not run in parallel only one core available/requested (dsims limits running in parallel to 1 less than the number of cores on the machine).", immediate. = TRUE, call. = FALSE)
        run.parallel = FALSE
      }
    }
  }
  if(run.parallel){
    # there is an issue with parallel on some machines for some versions of R and R studio
    ## WORKAROUND: https://github.com/rstudio/rstudio/issues/6692
    ## Revert to 'sequential' setup of PSOCK cluster in RStudio v1.3.959 or lower on macOS with R v4.0.0 or higher
    if (Sys.getenv("RSTUDIO") == "1" && !nzchar(Sys.getenv("RSTUDIO_TERM")) &&
        Sys.info()["sysname"] == "Darwin" && getRversion() >= "4.0.0") {
      if(rstudioapi::versionInfo()$version < "1.3.1056"){
        #warning(paste("The combination of versions of R-studio and R you are using on your mac may lead to an error when running in parallel. Please run the following command to try to correct this before running the simulation again (alternatively try updating R-studio): parallel:::setDefaultClusterOptions(setup_strategy = \"sequential\")", sep = ""), immediate. = TRUE, call. = FALSE)
        eval(parse(text = "parallel:::setDefaultClusterOptions(setup_strategy = \"sequential\")"))
      }
    }
    # intitialise the cluster
    myCluster <- parallel::makeCluster(nCores)
    parallel::clusterEvalQ(myCluster, {
      require(dsims)
    })
    on.exit(stopCluster(myCluster))
    if(counter){
        results <- pbapply::pblapply(X= as.list(1:simulation@reps), FUN = single.sim.loop, simulation = simulation, save.data = save.data, load.data = load.data, data.path = data.path, cl = myCluster, counter = FALSE)
    }else{
      results <- parLapply(myCluster, X = as.list(1:simulation@reps), fun = single.sim.loop, simulation = simulation, save.data = save.data, load.data = load.data, data.path = data.path, counter = FALSE)
    }
    #Extract results and warnings
    sim.results <- sim.warnings <- list()
    for(i in seq(along = results)){
      sim.results[[i]] <- results[[i]]$results
      sim.warnings[[i]] <- results[[i]]$warnings
    }
    simulation <- accumulate.PP.results(simulation = simulation, results = sim.results)
    simulation@warnings <- accumulate.warnings(sim.warnings)
    stopCluster(myCluster)
    on.exit()
  }
  if(!run.parallel){
    #otherwise loop
    for(i in 1:simulation@reps){
      results <- single.sim.loop(i = i,
                                 simulation = simulation,
                                 save.data = save.data,
                                 load.data = load.data,
                                 data.path = data.path,
                                 counter = counter,
                                 single.transect = FALSE,
                                 transect.path = character(0),
                                 save.transects = FALSE)
      simulation@results <- results$results
      simulation@warnings <- results$warnings
    }
  }
  simulation@results <- add.summary.results(results = simulation@results,
                                            model.count = length(simulation@ds.analysis@dfmodel))
  #Process warnings
  if(length(simulation@warnings$message) > 0){
    message("Summary of warnings and errors:")
    for(i in seq(along = simulation@warnings$message)){
      message(paste(simulation@warnings$message[[i]], " (occurred ", simulation@warnings$counter[[i]], " times)"))
    }
    message("-----")
  }

  return(simulation)
}

