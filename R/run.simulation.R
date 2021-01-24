#' Method to run a simulation
#'
#' Runs the simulation and returns the simulation object with results. If
#' running in parallel and max.cores is not specified it will default to using
#' one less than the number of cores / threads on your machine.
#'
#' @param simulation an object of class Simulation
#' @param run.parallel logical option to use multiple processors
#' @param max.cores integer maximum number of cores to use, if not specified then
#' one less than the number available will be used.
#' @param save.data logical allows the datasets from the simulation to be
#' saved to file
#' @param load.data logical allows the datasets to be loaded from file rather than
#' simulated afresh.
#' @param data.path character file path to the data files.
#' @param counter logical indicates if you would like to see the progress counter.
#' @param progress.file character file to output progress to for Distance for Windows
#' @param ... allows the five previous optional arguments to be specified
#' @return an object of class simulation which now includes the results
#' @export
#' @rdname run-methods
#' @seealso \code{\link{make.simulation}}

run.simulation <- function(object, run.parallel = FALSE, max.cores = NA, save.data = FALSE,
                           load.data = FALSE, data.path = character(), counter = TRUE,
                           progress.file = character(), ...){
  #Reset results arrays
  object@results <- create.results.arrays(object@reps,
                                          object@design@region,
                                          object@ds.analysis,
                                          object@population.description)
  #reset the error/warning message
  test <- try(object@warnings, silent = TRUE)
  if(class(test) == "list"){
    object@warnings$message <- list()
    object@warnings$counter <- list()
  }
  #check the data.path ends in "/"
  if(length(data.path) > 0){
    temp.path <- strsplit(data.path, split = "")
    if(temp.path[length(temp.path)] != "/"){
      # if not add it
      data.path <- paste(data.path, "/", sep = "")
    }
    rm(temp.path)
  }
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
    # intitialise the cluster
    myCluster <- makeCluster(nCores)
    clusterEvalQ(myCluster, {
      require(DSsim)
      require(shapefiles)
    })
    on.exit(stopCluster(myCluster))
    if(counter){
      if(length(progress.file) > 0){
        # Set up progress file
        cat(0, file = progress.file)
        results <- pbapply::pblapply(X = as.list(1:object@reps), FUN = single.simulation.loop, object = object, save.data = save.data, load.data = load.data, data.path = data.path, cl = myCluster, counter = TRUE, progress.file = progress.file, in.parallel = TRUE)
      }else{
        results <- pbapply::pblapply(X= as.list(1:object@reps), FUN = single.simulation.loop, object = object, save.data = save.data, load.data = load.data, data.path = data.path, cl = myCluster, counter = FALSE)
      }
    }else{
      results <- parLapply(myCluster, X = as.list(1:object@reps), fun = single.simulation.loop, object = object, save.data = save.data, load.data = load.data, data.path = data.path, counter = FALSE)
    }
    #Extract results and warnings
    sim.results <- sim.warnings <- list()
    for(i in seq(along = results)){
      sim.results[[i]] <- results[[i]]$results
      sim.warnings[[i]] <- results[[i]]$warnings
    }
    object <- accumulate.PP.results(simulation = object, results = sim.results)
    object@warnings <- accumulate.warnings(sim.warnings)
    stopCluster(myCluster)
    on.exit()
  }
  if(!run.parallel){
    #otherwise loop
    for(i in 1:object@reps){
      results <- single.simulation.loop(i, object, save.data = save.data, load.data = load.data, data.path = data.path, counter = counter, progress.file = progress.file)
      object@results <- results$results
      object@warnings <- results$warnings
    }
  }
  object@results <- add.summary.results(object@results, length(object@ddf.analyses))
  object@design@file.index <- orig.file.index
  #Process warnings
  test <- try(object@warnings, silent = TRUE)
  if(class(test) == "list"){
    if(length(object@warnings$message) > 0){
      message("Summary of warnings and errors:")
      for(i in seq(along = object@warnings$message)){
        message(paste(object@warnings$message[[i]], " (occurred ", object@warnings$counter[[i]], " times)"))
      }
      message("-----")
    }
  }
  return(object)
}

