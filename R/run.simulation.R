#' Method to run a simulation
#'
#' Runs the simulation and returns the simulation object with results. By
#' default the repetitions are run in parallel using one less than the number
#' of cores available on your machine. For example code see
#' \code{\link{make.simulation}}
#'
#' @section Parallel processing:
#' Repetitions are distributed to worker processes using the mirai package,
#' with each worker picking up a new repetition as soon as it finishes one.
#' The number of workers defaults to
#' \code{parallelly::availableCores(omit = 1)}, which respects limits set by
#' HPC schedulers and containers. If you have already started mirai daemons
#' (e.g. \code{mirai::daemons(8)}, or remote / HPC daemons) these are used
#' instead and are left running afterwards. Otherwise workers are started and
#' stopped on every call, which takes a few seconds; when running many short
#' simulations (e.g. in a loop) start the daemons once with
#' \code{mirai::daemons(n)} beforehand and stop them with
#' \code{mirai::daemons(0)} when finished.
#'
#' The workers are separate R processes (not forks). A population generator
#' (see \code{\link{make.simulation}}) is sent to them with its enclosing
#' environment, but objects in the global environment and packages it uses
#' are not: see the 'Population generator' section of
#' \code{\link{make.simulation}}.
#'
#' Each repetition uses its own random number stream, so after calling
#' \code{set.seed} results are identical whether run in serial or in parallel,
#' regardless of the number of workers.
#'
#' If a repetition fails with an error the simulation carries on; the
#' repetition is skipped and the error is reported in the summary of warnings
#' and errors.
#'
#' @param simulation \code{\link{Simulation-class}} object
#' @param run.parallel logical option to use multiple processors (default
#' TRUE). Ignored if only one core is available.
#' @param max.cores integer maximum number of cores to use, if not specified then
#' one less than the number available will be used.
#' @param counter logical indicates if you would like to see the progress
#' counter, which also shows the elapsed and estimated remaining time.
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
#' @importFrom mirai daemons daemons_set everywhere mirai mirai_map unresolved is_error_value is_mirai_error
#' @importFrom parallelly availableCores
#' @importFrom parallel nextRNGStream
#' @rdname run.simulation-methods
#' @seealso \code{\link{make.simulation}}
#' 
run.simulation <- function(simulation, run.parallel = TRUE, max.cores = NA, 
                         counter = TRUE, transect.path = character(0), 
                         progress.file = character(0)) {
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
  simulation@results <- create.results.arrays(simulation@reps,
                                              simulation@design@region,
                                              simulation@ds.analysis,
                                              simulation@population.description,
                                              population.generator = get.population.generator(simulation))
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
  # Give each rep its own random number stream so results are reproducible
  # whether run in serial or in parallel
  rep.seeds <- make.rep.seeds(simulation@reps)
  rep.args <- list(simulation = simulation,
                   save.data = save.data,
                   load.data = load.data,
                   data.path = data.path,
                   transect.path = transect.path,
                   rep.seeds = rep.seeds)
  if(run.parallel){
    compute <- launch.daemons(max.cores)
    if(is.null(compute)){
      run.parallel <- FALSE
    }else if(compute == "dsims"){
      # Only shut down the daemons dsims started
      on.exit(mirai::daemons(0, .compute = "dsims"), add = TRUE)
    }
  }
  if(run.parallel){
    tasks <- mirai::mirai_map(seq_len(simulation@reps), run.single.rep,
                              .args = rep.args, .compute = compute)
    wait.for.reps(tasks, simulation@reps, counter, progress.file)
    results <- tasks[]
  }else{
    old.seed <- get(".Random.seed", envir = globalenv())
    results <- vector("list", simulation@reps)
    start.time <- Sys.time()
    for(i in seq_len(simulation@reps)){
      results[[i]] <- tryCatch(do.call(run.single.rep, c(list(i), rep.args)),
                               error = function(e) e)
      report.progress(i, simulation@reps, start.time, counter, progress.file)
    }
    assign(".Random.seed", old.seed, envir = globalenv())
  }
  # Reps which failed with an error are skipped and the error recorded
  for(i in seq_along(results)){
    if(inherits(results[[i]], "error") || mirai::is_error_value(results[[i]])){
      results[[i]] <- list(warnings = message.handler(list(), paste("Error in repetition, repetition skipped:", failed.rep.message(results[[i]])), i))
    }
  }
  #Extract results and warnings
  sim.warnings <- lapply(results, function(x) x$warnings)
  simulation <- accumulate.PP.results(simulation = simulation, results = results)
  simulation@warnings <- accumulate.warnings(sim.warnings)
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

run.single.rep <- function(i, simulation, save.data, load.data, data.path,
                           transect.path, rep.seeds){
  # Runs repetition i using its own random number stream. Defined at the top
  # level so that it is sent to the daemons by reference to the dsims namespace.
  assign(".Random.seed", rep.seeds[[i]], envir = globalenv())
  single.sim.loop(i = i,
                  simulation = simulation,
                  save.data = save.data,
                  load.data = load.data,
                  data.path = data.path,
                  transect.path = transect.path,
                  save.transects = FALSE)
}

make.rep.seeds <- function(reps){
  # Returns a list of L'Ecuyer-CMRG seeds, one independent stream per rep.
  # The base seed is drawn from the current RNG so set.seed() controls the
  # streams, the user's RNG kind and state are then restored.
  base.seed <- sample.int(.Machine$integer.max, 1L)
  old.seed <- get(".Random.seed", envir = globalenv())
  old.kind <- RNGkind()
  on.exit({
    RNGkind(old.kind[1], old.kind[2], old.kind[3])
    assign(".Random.seed", old.seed, envir = globalenv())
  })
  RNGkind("L'Ecuyer-CMRG")
  set.seed(base.seed)
  seed <- get(".Random.seed", envir = globalenv())
  seeds <- vector("list", reps)
  for(i in seq_len(reps)){
    seeds[[i]] <- seed
    seed <- parallel::nextRNGStream(seed)
  }
  seeds
}

launch.daemons <- function(max.cores){
  # Returns the mirai compute profile to use, or NULL to run in serial.
  # Daemons the user has already set up (e.g. remote or HPC) are used as they
  # are, otherwise local daemons are started under the "dsims" profile.
  if(mirai::daemons_set()){
    check.daemon.version("default")
    return("default")
  }
  nCores <- parallelly::availableCores(omit = 1)
  if(!is.na(max.cores)){
    nCores <- min(nCores, max.cores)
  }
  if(nCores <= 1){
    warning("Could not run in parallel only one core available/requested (dsims limits running in parallel to 1 less than the number of cores on the machine).", immediate. = TRUE, call. = FALSE)
    return(NULL)
  }
  mirai::daemons(nCores, .compute = "dsims")
  # Daemons start with the default library paths, use this session's instead
  # so they load the same version of dsims
  # (a pre-built call, so the paths are sent as values)
  lib.call <- call(".libPaths", .libPaths())
  mirai::everywhere(lib.call, .min = nCores, .compute = "dsims")
  check.daemon.version("dsims")
  "dsims"
}

check.daemon.version <- function(compute){
  # Stops if the daemons load a different version of dsims to this session,
  # as every rep would then fail
  daemon.version <- mirai::mirai(as.character(utils::packageVersion("dsims")),
                                 .compute = compute)[]
  if(mirai::is_error_value(daemon.version)){
    if(compute == "dsims"){
      mirai::daemons(0, .compute = compute)
    }
    stop("Could not load dsims on the parallel workers: ", failed.rep.message(daemon.version), call. = FALSE)
  }
  if(daemon.version != as.character(utils::packageVersion("dsims"))){
    if(compute == "dsims"){
      mirai::daemons(0, .compute = compute)
    }
    stop("The parallel workers use dsims version ", daemon.version, " but this session uses version ", utils::packageVersion("dsims"), ". Please make sure the same version is installed for the workers.", call. = FALSE)
  }
}

wait.for.reps <- function(tasks, reps, counter, progress.file){
  # Polls the parallel tasks, reporting progress as reps complete
  start.time <- Sys.time()
  done <- 0
  repeat{
    now.done <- sum(!vapply(tasks, mirai::unresolved, logical(1)))
    if(now.done != done){
      done <- now.done
      report.progress(done, reps, start.time, counter, progress.file)
    }
    if(done == reps){
      break
    }
    Sys.sleep(0.5)
  }
}

report.progress <- function(done, reps, start.time, counter, progress.file){
  # Displays the progress counter with elapsed / estimated remaining time and
  # writes the progress (integer %) to file for Distance for Windows
  if(counter){
    elapsed <- as.numeric(difftime(Sys.time(), start.time, units = "secs"))
    remaining <- elapsed / done * (reps - done)
    message("\r", done, " out of ", reps, " reps (elapsed ", duration.string(elapsed),
            ", remaining ~", duration.string(remaining), ")     ",
            appendLF = done == reps)
  }
  if(length(progress.file) > 0){
    try(cat(round(done/reps*100), file = progress.file), silent = TRUE)
  }
}

duration.string <- function(secs){
  # Formats a number of seconds as e.g. "1d 02h 05m", "2h 05m" or "4m 09s"
  secs <- round(secs)
  d <- secs %/% 86400
  h <- (secs %% 86400) %/% 3600
  m <- (secs %% 3600) %/% 60
  if(d > 0) return(sprintf("%dd %02dh %02dm", d, h, m))
  if(h > 0) return(sprintf("%dh %02dm", h, m))
  sprintf("%dm %02ds", m, secs %% 60)
}

failed.rep.message <- function(err){
  # Extracts the message from an R error or a mirai error value
  if(inherits(err, "error")){
    return(conditionMessage(err))
  }
  if(mirai::is_mirai_error(err)){
    return(attr(err, "message"))
  }
  paste("the worker process failed (mirai error value ", as.integer(err), ")", sep = "")
}
