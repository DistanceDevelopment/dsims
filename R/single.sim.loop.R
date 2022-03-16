#' @importFrom utils flush.console
#' @importFrom methods new is
#' @importFrom mrds dht
#single.simulation.loop <- function(i, object){
single.sim.loop <- function(i, simulation, save.data, load.data, data.path = character(0), counter, in.parallel = FALSE, single.transect = FALSE, transect.path = character(0), save.transects = FALSE){
  # Input: i - integer representing the loop number
  #        simulation - an simulation of class Simulation
  #
  # Output: list of simulation results and warnings
  #
  # Display/write to file the progress of the simulation
  if(counter && !in.parallel){
    # Write to terminal
    message("\r", i, " out of ", simulation@reps,  " reps     \r", appendLF = FALSE)
  }
  flush.console()
  if(!load.data){
    #generate population
    population <- generate.population(simulation)
    #generate transects
    load.transects <- FALSE
    if(length(transect.path) > 0 && !save.transects){
      # Set flag to true
      load.transects <- TRUE
      stop("Loading transects from shapefile is not yet implemented.")
      if(single.transect){
        # read in from transect.path
        transect.filename <- transect.path
      }else{
        # Get all shapefile filenames and find the correct file for iteration i
        # transect.filename <- get.filename.i(transect.path, i)
      }
    }else{
      transects <- generate.transects(simulation)
      if(save.transects){
        stop("Saving transects to shapefile is not yet implemented.")
        # Add code to write transects shapefile or other!
      }
    }
    #make survey object
    if(inherits(transects, "Line.Transect")){
      # Check transects for empty geometries
      transects <- check.transects(transects)
      survey <- new(Class = "Survey.LT", population = population, transect = transects, perp.truncation = simulation@detectability@truncation)
    }else if(inherits(transects, "Point.Transect")){
      survey <- new(Class = "Survey.PT", population = population, transect = transects, rad.truncation = simulation@detectability@truncation)
    }
  }
  #Load or generate survey data
  if(load.data){
    #load data
    load(paste(data.path,"survey_",i,".robj", sep = ""))
    if(inherits(survey, "Survey")){

      dists.in.covered <- survey@dists.in.covered
    }

  }else{
    #simulate survey
    survey <- run.survey(object = survey, region = simulation@design@region)
    dist.data <- survey@dist.data
    if(nrow(dist.data) > 0){
      dists.in.covered <- survey@dists.in.covered
      # Need to unflatten data for some checks and functions
      data.tables <- Distance::unflatten(dist.data)
      dht.dists <- data.tables$data
      region.table <- data.tables$region.table
      sample.table <- data.tables$sample.table
      obs.table <- data.tables$obs.table
    }
    # Check if we have to save the data
    if(save.data){
      save(survey, file = paste(data.path,"survey_",i,".robj", sep = ""))
    }
  }
  #Find how many animals were in the covered region
  truncation.list <- simulation@ds.analysis@truncation
  if(length(truncation.list) > 0){
    if(length(truncation.list[[1]]) == 1){
      if(is.double(truncation.list[[1]])){
        right <- truncation.list[[1]]
      }else{
        right.p <- as.numeric(sub("%","",simulation@ds.analysis@truncation[[1]]))
        right <- quantile(na.omit(dist.data$distance), probs=1-right.p/100, na.rm=TRUE)
      }
      left <- 0
    }else{
      if(is.double(truncation.list[[1]]$right)){
        right <- truncation.list[[1]]$right
      }else{
        right.p <- as.numeric(sub("%","",simulation@ds.analysis@truncation[[1]]$right))
        right <- quantile(na.omit(dist.data$distance), probs=1-right.p/100, na.rm=TRUE)
      }
      if(is.double(truncation.list[[1]]$left)){
        left <- truncation.list[[1]]$left
      }else{
        left.p <- as.numeric(sub("%","",simulation@ds.analysis@truncation[[1]]$left))
        left <- quantile(na.omit(dist.data$distance), probs=left.p/100, na.rm=TRUE)
      }
    }
    # Find how many data points are between the truncation distances
    dists.in.covered <- dists.in.covered[dists.in.covered >= left]
    dists.in.covered <- dists.in.covered[dists.in.covered <= right]
    n.in.covered <- length(dists.in.covered)
  }else{
    n.in.covered <- length(dists.in.covered)
  }
  #analyse survey if there are data to analyse
  if(nrow(dist.data[!is.na(dist.data$distance),]) >= 20){
    model.results <- analyse.data(simulation@ds.analysis, data.obj = survey, simulation@warnings, i = i)
    warnings <- model.results$warnings
    num.successful.models <- model.results$num.successful.models
    model.results <- model.results$model
  }else{
    warning("There are too few data points (<20) to be analysed, skipping this iteration.", call. = FALSE, immediate. = TRUE)
    model.results <- NULL
    warnings <- simulation@warnings
  }
  #Check at least one model worked
  if(!is.null(model.results)){
    #Store ddf results
    simulation@results$Detection <- store.ddf.results(simulation@results$Detection,
                                                      model.results$ddf,
                                                      i, n.in.covered,
                                                      num.successful.models)
    #Check to see if the stratification is to be modified for analysis
    analysis.strata <- simulation@ds.analysis@group.strata
    # Set recompute dht - done as part of ds but might need redone for grouped strata
    # or missing distances.
    recompute.dht <- FALSE
    if(nrow(analysis.strata) > 0){
      new.tables <- modify.strata.for.analysis(analysis.strata,
                                               obs.table,
                                               sample.table,
                                               region.table)
      obs.table <- new.tables$obs.table
      sample.table <- new.tables$sample.table
      region.table <- new.tables$region.table
      recompute.dht <- TRUE
    }

    #Check if there are missing distances - NA's may be because there are transects with no observations in flat file format! This is for two type detectors and not yet implemented in design engine.
    # miss.dists <- any(is.na(dist.data$distance))
    # if(miss.dists){
    #   # Add the missing distance observations in to ddf object
    #   missing.dists <- ddf.data@ddf.dat[is.na(ddf.data@ddf.dat$distance),]
    #   # NA's break dht
    #   missing.dists$distance <- rep(-1, nrow(missing.dists))
    #   if(is.null(missing.dists$detected)){
    #     missing.dists$detected <- rep(1, nrow(missing.dists))
    #   }
    #   model.results <- add.miss.dists(missing.dists, model.results)
    #   recompute.dht <- TRUE
    # }

    #Compute density / abundance estimates
    if(recompute.dht){
      # set dht options
      dht.options <- list()
      dht.options$ervar <- simulation@ds.analysis@er.var
      dht.results <- try(dht(model.results$ddf,
                             region.table,
                             sample.table,
                             obs.table,
                             options = dht.options), silent = TRUE)
      if(is(dht.results, "try-error")){
        warning(paste("Problem", strsplit(dht.results[1], "Error")[[1]][2], " dht results not being recorded for iteration ", i, sep=""), call. = FALSE, immediate. = TRUE)
      }else{
        simulation@results <- store.dht.results(simulation@results,
                                                dht.results, i,
                                                simulation@population.description@size,
                                                dist.data,
                                                obs.table,
                                                sample.table)
      }
    }
    else{
      # Just store existing ds dht results
      clusters <- "size" %in% names(dist.data)
      simulation@results <- store.dht.results(results = simulation@results,
                                              dht.results = model.results$dht,
                                              i = i,
                                              clusters = clusters,
                                              data = dist.data,
                                              obs.tab = obs.table,
                                              sample.tab = sample.table)
    }
  }
  # If the transects were loaded store the filename in the results
  if(load.transects){
    simulation@results$filename <- transect.filename
  }
  return(list(results = simulation@results, warnings = warnings))
}

