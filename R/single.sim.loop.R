#' @importFrom utils flush.console
#single.simulation.loop <- function(i, object){
single.sim.loop <- function(i, object, save.data, load.data, data.path = character(0), counter, progress.file = "", in.parallel = FALSE, transect.file = character(0), save.transects = character(0)){
  # Input: i - integer representing the loop number
  #        object - an object of class Simulation
  #
  # Output: list of simulation results and warnings
  #
  # Display/write to file the progress of the simulation
  if(counter){
    if(progress.file == ""){
      # Write to terminal
      message("\r", i, " out of ", object@reps,  " reps     \r", appendLF = FALSE)
    }else{
      # Calculate progress as an integer %
      progress <- round(i/object@reps*100)
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
    population <- generate.population(object)
    #generate transects
    if(length(transect.file) > 0 && length(save.transects) == 0){
      #read transect info from file (options for shapefiles and transect objects!)
      #*** - add code to read transects from shapefile or r obj (or other?)
    }else{
      transects <- generate.transects(object)
      if(length(save.transects) > 0){
        #*** - add code to write transects to R obj or shapefile or other!
      }
    }
    #make survey object
    if(inherits(transects, "Line.Transect")){
      survey <- new(Class = "Survey.LT", population = population, transect = transects, perp.truncation = object@detectability@truncation)
    }else if(inherits(transects, "Point.Transect")){
      survey <- new(Class = "Survey.PT", population = population, transect = transects, rad.truncation = object@detectability@truncation)
    }
  }
  #Load or generate survey data
  if(load.data){
    #load data
    load(paste(data.path,"survey_",i,".robj", sep = ""))
    dists.in.covered <- survey@dists.in.covered
  }else{
    #simulate survey
    survey <- run.survey(object = survey)
    dists.in.covered <- survey@dists.in.covered
    if(save.data){
      save(survey, file = paste(data.path,"survey_",i,".robj", sep = ""))
    }
  }
  #Find how many animals were in the covered region
  if(length(object@ddf.analyses[[1]]@truncation) > 0){
    n.in.covered <- length(which(dists.in.covered <= object@ddf.analyses[[1]]@truncation))
  }else{
    n.in.covered <- length(dists.in.covered)
  }
  #analyse survey if there are data to analyse
  if(nrow(ddf.data@ddf.dat[!is.na(ddf.data@ddf.dat$distance),]) >= 20){
    ddf.results <- run.analysis(survey)
    warnings <- ddf.results$warnings
    num.successful.models <- ddf.results$num.successful.models
    ddf.results <- ddf.results$best.model
  }else{
    warning("There are too few data points (<20) to be analysed, skipping this iteration.", call. = FALSE, immediate. = TRUE)
    ddf.results <- NULL
    warnings <- object@warnings
  }
  #Check at least one model worked
  if(!is.null(ddf.results)){
    #Store ddf results
    object@results$Detection <- store.ddf.results(object@results$Detection, ddf.results, i, n.in.covered, num.successful.models)
    #Check to see if the stratification is to be modified for analysis
    analysis.strata <- object@ddf.analyses[[1]]@analysis.strata
    if(nrow(analysis.strata) > 0){
      new.tables <- modify.strata.for.analysis(analysis.strata, obs.table, sample.table, region.table)
      obs.table <- new.tables$obs.table
      sample.table <- new.tables$sample.table
      region.table <- new.tables$region.table
    }
    #Check if there are missing distances
    miss.dists <- any(is.na(ddf.data@ddf.dat$distance))
    if(miss.dists){
      # Add the missing distance observations in to ddf object
      missing.dists <- ddf.data@ddf.dat[is.na(ddf.data@ddf.dat$distance),]
      # NA's break dht
      missing.dists$distance <- rep(-1, nrow(missing.dists))
      if(is.null(missing.dists$detected)){
        missing.dists$detected <- rep(1, nrow(missing.dists))
      }
      ddf.results <- add.miss.dists(missing.dists, ddf.results)
    }
    #Compute density / abundance estimates
    compute.dht = TRUE
    if(compute.dht){
      dht.options <- list()
      # if it is a point transect design
      if(inherits(object@design, "PT.Design")){
        dht.options$ervar <- "P3"
      }
      dht.results <- try(dht(ddf.results, region.table@region.table, sample.table@sample.table, obs.table@obs.table, options = dht.options), silent = TRUE)
      if(class(dht.results) == "try-error"){
        warning(paste("Problem", strsplit(dht.results[1], "Error")[[1]][2], " dht results not being recorded for iteration ", i, sep=""), call. = FALSE, immediate. = TRUE)
      }else{
        object@results <- store.dht.results(object@results, dht.results, i, object@population.description@size, ddf.data@ddf.dat, obs.table@obs.table)
      }
    }
  }
  object@results$filename <- object@design@filenames[object@design@file.index]
  return(list(results = object@results, warnings = warnings))
}

