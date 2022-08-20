#' @importFrom utils flush.console
#' @importFrom methods new is
#' @importFrom mrds dht
#single.simulation.loop <- function(i, object){
single.sim.loop <- function(i, simulation, save.data, load.data, data.path = character(0), counter, in.parallel = FALSE, transect.path = character(0), save.transects = FALSE, progress.file = character(0), debug = FALSE){
  # Input: i - integer representing the loop number
  #        simulation - an simulation of class Simulation
  #        save.data - logical whether to save the data
  #        load.data - logical whether to load the data
  #        data.path - where to load/save the data
  #        counter - whether to display a progress counter
  #        in.parallel - whether it is being run in parallel
  #        transect.path - vector of shapefile paths for each iteration or 
  #           locations to save them to (latter not yet implemented)
  #        save.transects - logical whether to save the transects 
  #        debug - gets the function to return more objects for
  #           debugging or testing
  #
  # Output: list of simulation results and warnings
  #
  warnings <- simulation@warnings
  # Display/write to file the progress of the simulation
  if(counter){
    if(length(progress.file) == 0 && !in.parallel){
      # Write to terminal
      message("\r", i, " out of ", simulation@reps,  " reps     \r", appendLF = FALSE)  
    }else if(length(progress.file) > 0){
      # Calculate progress as an integer %
      progress <- round(i/simulation@reps*100)
      # Check if being run in parallel
      if(in.parallel){
        #If so load file to check if progress should be updated 
        old.progress <- try(scan(progress.file, what=integer()), silent = TRUE)
        if(inherits(old.progress,"integer")){
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
    load.transects <- FALSE
    if(length(transect.path) > 0 && !save.transects){
      # Pick the correct filename for the rep
      filename <- transect.path[i] 
      # Set flag to true
      load.transects <- TRUE
      if(inherits(simulation@design, "Segment.Transect.Design")){
        transects <- read.seg.transects(filename = filename, 
                                        design = simulation@design, 
                                        warnings = simulation@warnings, 
                                        rep = i)
      }else if(inherits(simulation@design, "Line.Transect.Design")){
        transects <- read.line.transects(filename = filename, 
                                         design = simulation@design, 
                                         warnings = simulation@warnings, 
                                         rep = i)
      }else{
        transects <- read.point.transects(filename = filename, 
                                          design = simulation@design, 
                                          warnings = simulation@warnings, 
                                          rep = i)
      }
      warnings <- transects$warnings
      transects <- transects$transects
      if(is.null(transects)){
        return(list(results = simulation@results, warnings = warnings))
      }
      simulation@results$filename[i] <- filename
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
  if(nrow(dist.data[!is.na(dist.data$distance),]) > 0){
    model.results <- analyse.data(simulation@ds.analysis, data.obj = survey, simulation@warnings, i = i)
    warnings <- model.results$warnings
    num.successful.models <- model.results$num.successful.models
    model.results <- model.results$model
  }else{
    warnings <- message.handler(warnings, paste("There were no detections, iteration skipped.", sep = ""), i)
    model.results <- NULL
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
        tmp.results <- try(store.dht.results(simulation@results,
                                             dht.results, i,
                                             simulation@population.description@size,
                                             dist.data,
                                             obs.table,
                                             sample.table), silent = TRUE)
      }
    }
    else{
      # Just store existing ds dht results
      clusters <- "size" %in% names(dist.data)
      tmp.results <- try(store.dht.results(results = simulation@results,
                                           dht.results = model.results$dht,
                                           i = i,
                                           clusters = clusters,
                                           data = dist.data,
                                           obs.tab = obs.table,
                                           sample.tab = sample.table), silent = TRUE)
    }
    if(is(tmp.results, "try-error")){
      warnings <- message.handler(warnings, paste("Error in storing dht results, iteration skipped.", sep = ""), i)
    }else{
      simulation@results <- tmp.results
    }
  }
  if(debug){
    return(list(results = simulation@results, 
                survey = survey,
                warnings = warnings))
  }
  return(list(results = simulation@results, warnings = warnings))
}

