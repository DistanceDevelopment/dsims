#' @importFrom utils flush.console
#single.simulation.loop <- function(i, object){
single.sim.loop <- function(i, simulation, save.data, load.data, data.path = character(0), counter, progress.file = "", in.parallel = FALSE, single.transect = FALSE, transect.file = character(0), save.transects = character(0)){
  # Input: i - integer representing the loop number
  #        simulation - an simulation of class Simulation
  #
  # Output: list of simulation results and warnings
  #
  # Display/write to file the progress of the simulation
  if(counter){
    if(progress.file == ""){
      # Write to terminal
      message("\r", i, " out of ", simulation@reps,  " reps     \r", appendLF = FALSE)
    }else{
      # Calculate progress as an integer %
      progress <- round(i/simulation@reps*100)
      # Check if being run in parallel
      if(in.parallel){
        #If so load file to check if progress should be updated
        old.progress <- try(scan(progress.file, what=integer()), silent = TRUE)
        if(class(old.progress) == "integer"){
          #Only update if this is the latest progress (when running in parallel things may not be processed in exactly the right order)
          if(progress > old.progress){
            try(cat(progress, file = progress.file), silent = TRUE)
          }
        }
      }else{
        cat(progress, file = progress.file)
      }
    }
  }
  flush.console()
  if(!load.data){
    #generate population
    population <- generate.population(simulation)
    #generate transects
    if(length(transect.file) > 0 && length(save.transects) == 0){
      #read transect info from file (options for shapefiles and transect objects!)
      #*** - add code to read transects from shapefile or r obj (or other?)
    }else{
      transects <- generate.transects(simulation)
      if(length(save.transects) > 0){
        #*** - add code to write transects to R obj or shapefile or other!
      }
    }
    #make survey object
    if(class(transects) == "Line.Transect"){
      survey <- new(Class = "Survey.LT", population = population, transect = transects, perp.truncation = simulation@detectability@truncation)
    }else if(class(transects) == "Point.Transect"){
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
    survey <- run.survey(object = survey)
    dist.data <- survey@dist.data
    dists.in.covered <- survey@dists.in.covered
    if(save.data){
      save(survey, file = paste(data.path,"survey_",i,".robj", sep = ""))
    }
  }
  #Find how many animals were in the covered region
  if(length(simulation@ds.analysis@truncation) > 0){
    n.in.covered <- length(which(dists.in.covered <= simulation@ds.analysis@truncation))
  }else{
    n.in.covered <- length(dists.in.covered)
  }
  #analyse survey if there are data to analyse
  if(nrow(dist.data[!is.na(dist.data$distance),]) >= 20){
    model.results <- analyse.data(simulation@ds.analysis, survey)
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
    if(nrow(analysis.strata) > 0){
      # Need to first make dht data tables (function in Distance)***
      new.tables <- modify.strata.for.analysis(analysis.strata, obs.table, sample.table, region.table)
      obs.table <- new.tables$obs.table
      sample.table <- new.tables$sample.table
      region.table <- new.tables$region.table
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
    # }

    #Compute density / abundance estimates
    compute.dht = TRUE
    if(compute.dht){
      dht.options <- list()
      # if it is a point transect design
      if(inherits(simulation@design, "PT.Design")){
        dht.options$ervar <- "P3"
      }
      dht.results <- try(dht(model.results, region.table@region.table, sample.table@sample.table, obs.table@obs.table, options = dht.options), silent = TRUE)
      if(class(dht.results) == "try-error"){
        warning(paste("Problem", strsplit(dht.results[1], "Error")[[1]][2], " dht results not being recorded for iteration ", i, sep=""), call. = FALSE, immediate. = TRUE)
      }else{
        simulation@results <- store.dht.results(simulation@results, dht.results, i, simulation@population.description@size, ddf.data@ddf.dat, obs.table@obs.table)
      }
    }
  }
  simulation@results$filename <- simulation@design@filenames[simulation@design@file.index]
  return(list(results = simulation@results, warnings = warnings))
}

