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
#' @param transect.path character gives the pathway to a folder of shapefiles or
#' the path to a single shapefile (.shp file) which give the transects which should
#' be used for the simulations. If a folder of transects a new shapefile will be
#' used for each repetition. If a path specifying a single shapefile then the same
#' transects will be used for each repetition.
#' @param progress.file character path with filename to output progress to file
#' for Distance for Windows progress counter. Not to be used when running directly
#' in R.
#' @return the \code{\link{Simulation-class}} object which now includes
#' the results
#' @export
#' @importFrom parallel detectCores makeCluster clusterEvalQ stopCluster parLapplyLB clusterExport
#' @importFrom rstudioapi versionInfo
#' @rdname run.simulation-methods
#' @seealso \code{\link{make.simulation}}
run.simulation <- function(simulation, run.parallel = FALSE, max.cores = NA, counter = TRUE, transect.path = character(0), progress.file = character(0)){
  save.data <- load.data <- FALSE
  data.path <- character()
  # Check if the transect path ends in / if so remove - hard to do the same for windows!
  if(length(transect.path) > 0){
    if(substr(transect.path, nchar(transect.path), nchar(transect.path)) == "/"){
      transect.path <- substr(transect.path, 0, nchar(transect.path)-1)
    }
  }
  transect.path.master <- transect.path
  # Check if it is a single transect set or a folder
  if(length(transect.path) > 0){
    # Check if a folder or file have been specified
    check.path <- unlist(strsplit(transect.path, split = "[.]"))
    index <- length(check.path)
    if(check.path[index] == "shp"){
      # Set transect path to repeat the same value for each rep (for running in parallel)
      transect.path <- rep(transect.path, simulation@reps)
    }else{
      # Make up the vector of shapefile paths
      transect.path <- file.path(transect.path, list.files(path = transect.path, pattern = "shp"))
      if(length(transect.path) == 0){
        stop(paste("No shapefiles found at ", transect.path.master,", cannot run simulation.", sep = ""), call. = FALSE)
      }
      if(length(transect.path) < simulation@reps){
        warning(paste("Insufficient transect shapefiles supplied (", length(transect.path)," supplied, ", simulation@reps, " required). Simulation will use some sets of transects more than once, this may influence the results.", sep = ""), immediate. = TRUE, call. = FALSE)
        mult.factor <- ceiling(simulation@reps/length(transect.path))
        transect.path <- rep(transect.path, mult.factor)[1:simulation@reps]
      }
    }
  }
  #Reset results arrays
  simulation@results <- create.results.arrays(simulation@reps,
                                              simulation@design@region,
                                              simulation@ds.analysis,
                                              simulation@population.description)
  #reset the error/warning message
  simulation@warnings$message <- list()
  simulation@warnings$counter <- list()
  simulation@warnings$index <- list()
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
    if(!requireNamespace('parallel', quietly = TRUE)){
      warning("Could not run in parallel, check parallel library is installed.", immediate. = TRUE, call. = FALSE)
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
    worker.state <- list(simulation = simulation,
                         save.data = save.data,
                         load.data = load.data,
                         data.path = data.path,
                         transect.path = transect.path,
                         save.transects = FALSE,
                         progress.file = progress.file)
    worker.fun <- function(i){
      state <- get(".dsims_worker_state", envir = .GlobalEnv)
      single.sim.loop(i = i,
                      simulation = state$simulation,
                      save.data = state$save.data,
                      load.data = state$load.data,
                      data.path = state$data.path,
                      counter = FALSE,
                      in.parallel = TRUE,
                      transect.path = state$transect.path,
                      save.transects = state$save.transects,
                      progress.file = state$progress.file)
    }
    parallel::clusterExport(myCluster,
                            varlist = c("worker.state", "worker.fun"),
                            envir = environment())
    parallel::clusterEvalQ(myCluster, {
      .dsims_worker_state <- worker.state
      NULL
    })
    on.exit(stopCluster(myCluster))
    if(counter){
      message("Parallel run uses load-balanced scheduling; per-repetition progress bar is disabled.")
    }
    results <- parallel::parLapplyLB(myCluster,
                                     X = as.list(1:simulation@reps),
                                     fun = worker.fun)
    #Extract results and warnings
    sim.warnings <- lapply(results, function(x) x$warnings)
    simulation <- accumulate.PP.results(simulation = simulation, results = results)
    simulation@warnings <- accumulate.warnings(sim.warnings)
    stopCluster(myCluster)
    on.exit()
  }
  if(!run.parallel){
    #otherwise loop
    sim.warnings <- vector("list", simulation@reps)
    for(i in 1:simulation@reps){
      results <- single.sim.loop(i = i,
                                 simulation = simulation,
                                 save.data = save.data,
                                 load.data = load.data,
                                 data.path = data.path,
                                 counter = counter,
                                 transect.path = transect.path,
                                 save.transects = FALSE,
                                 progress.file = progress.file)
      if(!is.null(results$rep.result)){
        simulation@results <- apply.rep.result(simulation@results,
                                               results$rep.result,
                                               i)
        if(!is.null(results$filename) && length(results$filename) > 0){
          simulation@results$filename[i] <- results$filename
        }
      }else{
        simulation@results <- results$results
      }
      sim.warnings[[i]] <- results$warnings
    }
    simulation@warnings <- accumulate.warnings(sim.warnings)
  }
  simulation@results <- add.summary.results(results = simulation@results,
                                            model.count = length(simulation@ds.analysis@dfmodel))
  #Process warnings
  if(length(simulation@warnings$message) > 0){
    message("Summary of warnings and errors:")
    for(i in seq(along = simulation@warnings$message)){
      rep.info <- ifelse(is.null(simulation@warnings$index), "",
                         paste(" in repetition(s): ", paste(simulation@warnings$index[[i]], collapse = ", ")))
      message(paste(simulation@warnings$message[[i]], " (occurred ", simulation@warnings$counter[[i]], " time(s)", rep.info, ")", sep = ""))
    }
    message("-----")
  }

  return(simulation)
}

