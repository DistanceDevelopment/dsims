# Helper functions for user supplied population generators (see the
# population.generator argument of make.simulation) and for recording
# repetitions with zero detections (see the zero.n argument of
# make.ds.analysis).

get.population.generator <- function(simulation){
  # Returns the population generator or NULL (also for Simulation objects
  # created with an earlier version of dsims which do not have the slot)
  if(!methods::.hasSlot(simulation, "population.generator")){
    return(NULL)
  }
  simulation@population.generator
}

get.zero.n <- function(ds.analysis){
  # Returns the zero detection option ("fail" for DS.Analysis objects created
  # with an earlier version of dsims)
  if(!methods::.hasSlot(ds.analysis, "zero.n") || length(ds.analysis@zero.n) == 0){
    return("fail")
  }
  ds.analysis@zero.n
}

run.population.generator <- function(simulation, i){
  # Calls the user supplied population generator for repetition i, saving and
  # restoring the random number generator state so the generator can use its
  # own random number stream without changing the stream used for the
  # transects, detections and analyses. Returns a list with the validated
  # population and the truth (NULL if not supplied).
  generator <- get.population.generator(simulation)
  global.env <- globalenv()
  had.seed <- exists(".Random.seed", envir = global.env, inherits = FALSE)
  if(had.seed){
    old.seed <- get(".Random.seed", envir = global.env, inherits = FALSE)
  }
  gen.output <- tryCatch({
    generator(simulation, i)
  }, error = function(e){
    e
  }, finally = {
    # .Random.seed also encodes the RNG kind so this restores any change of
    # kind made by the generator
    if(had.seed){
      assign(".Random.seed", old.seed, envir = global.env)
    }else if(exists(".Random.seed", envir = global.env, inherits = FALSE)){
      rm(".Random.seed", envir = global.env)
    }
  })
  if(inherits(gen.output, "error")){
    stop("population.generator failed in repetition ", i, ": ",
         conditionMessage(gen.output), call. = FALSE)
  }
  # Separate the population from the truth
  truth <- NULL
  if(is(gen.output, "Population")){
    population <- gen.output
  }else if(is.list(gen.output) && !is.null(gen.output$population)){
    population <- gen.output$population
    truth <- gen.output$truth
    unknown <- setdiff(names(gen.output), c("population", "truth"))
    if(length(unknown) > 0){
      stop("population.generator returned a list with unknown element(s) '",
           paste(unknown, collapse = "', '"), "' in repetition ", i,
           ", only 'population' and 'truth' are allowed.", call. = FALSE)
    }
  }else{
    stop("population.generator must return a Population object or a list(population = <Population>, truth = <named list>), it returned an object of class '",
         paste(class(gen.output), collapse = "/"), "' in repetition ", i, ".", call. = FALSE)
  }
  check.generated.population(population, simulation, i)
  truth <- check.generated.truth(truth, i)
  list(population = population, truth = truth)
}

check.generated.population <- function(population, simulation, i){
  # Checks that a population returned by a population generator can be used
  # by the rest of the simulation, stops with an informative error otherwise.
  fail <- function(...){
    stop("Invalid population returned by population.generator in repetition ",
         i, ": ", ..., call. = FALSE)
  }
  if(!is(population, "Population")){
    fail("expected an object of class 'Population' but got class '",
         paste(class(population), collapse = "/"), "'.")
  }
  if(!is(population@detectability, "Detectability")){
    fail("the detectability slot must contain a 'Detectability' object.")
  }
  pop.data <- population@population
  if(!is.data.frame(pop.data)){
    fail("the population slot must be a data.frame.")
  }
  region <- simulation@design@region
  # Required columns
  required <- c("individual", "x", "y", "Region.Label")
  if(nrow(pop.data) > 0){
    required <- c(required, "scale.param")
    if(population@detectability@key.function == "hr"){
      required <- c(required, "shape.param")
    }
    # Covariates used in the analyses
    analysis.vars <- unique(unlist(lapply(simulation@ds.analysis@dfmodel, all.vars)))
    required <- c(required, analysis.vars)
    # Cluster size if the population description has clusters
    if(simulation@population.description@size){
      required <- c(required, "size")
    }
  }
  missing.cols <- setdiff(unique(required), names(pop.data))
  if(length(missing.cols) > 0){
    fail("the population data.frame is missing the column(s) '",
         paste(missing.cols, collapse = "', '"),
         "'. Populations made by generate.population() contain all required columns.")
  }
  if(nrow(pop.data) == 0){
    return(invisible(TRUE))
  }
  for(col in intersect(c("x", "y", "scale.param", "shape.param"), names(pop.data))){
    if(!is.numeric(pop.data[[col]]) || any(is.na(pop.data[[col]]))){
      fail("column '", col, "' must be numeric with no missing values.")
    }
  }
  if(any(duplicated(pop.data$individual))){
    fail("the values in column 'individual' must be unique.")
  }
  # Strata names
  strata.names <- region@strata.name
  if(length(strata.names) == 0){
    strata.names <- region@region.name
  }
  bad.strata <- setdiff(unique(as.character(pop.data$Region.Label)), strata.names)
  if(length(bad.strata) > 0){
    fail("Region.Label value(s) '", paste(bad.strata, collapse = "', '"),
         "' do not match the strata names of the design region ('",
         paste(strata.names, collapse = "', '"), "').")
  }
  # Locations must be inside the design region (in the stratum given by
  # Region.Label)
  region.sf <- region@region
  for(strat in unique(as.character(pop.data$Region.Label))){
    index <- which(as.character(pop.data$Region.Label) == strat)
    poly.index <- ifelse(length(strata.names) > 1, match(strat, strata.names), 1)
    pts <- sf::st_as_sf(pop.data[index, c("x", "y")], coords = c("x", "y"),
                        crs = sf::st_crs(region.sf))
    inside <- lengths(sf::st_intersects(pts, sf::st_geometry(region.sf)[poly.index])) > 0
    if(!all(inside)){
      fail(sum(!inside), " individual(s) with Region.Label '", strat,
           "' lie outside this stratum of the design region (e.g. individual ",
           pop.data$individual[index[which(!inside)[1]]],
           "). Only individuals inside the design region should be returned.")
    }
  }
  invisible(TRUE)
}

check.generated.truth <- function(truth, i){
  # Checks the truth returned by a population generator and returns it as a
  # named numeric vector with entries N and D (NA if not supplied) or NULL.
  if(is.null(truth)){
    return(NULL)
  }
  if(is.list(truth)){
    if(!all(vapply(truth, function(x) is.numeric(x) && length(x) == 1, logical(1)))){
      stop("The truth returned by population.generator in repetition ", i,
           " must contain single numeric values.", call. = FALSE)
    }
    truth <- unlist(truth)
  }
  if(!is.numeric(truth) || is.null(names(truth)) || any(names(truth) == "") ||
     !all(names(truth) %in% c("N", "D")) || any(duplicated(names(truth)))){
    stop("The truth returned by population.generator in repetition ", i,
         " must be a named list or vector with numeric entries 'N' and/or 'D'.", call. = FALSE)
  }
  if(any(!is.finite(truth)) || any(truth < 0)){
    stop("The truth returned by population.generator in repetition ", i,
         " must contain finite, non-negative values.", call. = FALSE)
  }
  out <- c(N = NA_real_, D = NA_real_)
  out[names(truth)] <- truth
  out
}

results.strata.map <- function(results.strata, region, group.strata){
  # Returns a list with one element per row of the results arrays giving the
  # design strata names which make up that row
  design.strata <- region@strata.name
  if(length(design.strata) <= 1){
    out <- list(design.strata)
    names(out) <- results.strata
    return(out)
  }
  out <- list()
  for(row.name in results.strata){
    if(row.name == "Total"){
      out[[row.name]] <- design.strata
    }else if(nrow(group.strata) > 0){
      out[[row.name]] <- as.character(group.strata$design.id[group.strata$analysis.id == row.name])
    }else{
      out[[row.name]] <- row.name
    }
  }
  out
}

store.true.N <- function(results, population, region, group.strata, i){
  # Stores the realised number of individuals (and clusters) in the design
  # region in repetition i
  if(is.null(results$individuals$true.N)){
    return(results)
  }
  pop.data <- population@population
  strata.map <- results.strata.map(dimnames(results$individuals$true.N)[[1]],
                                   region, group.strata)
  labels <- as.character(pop.data$Region.Label)
  has.size <- "size" %in% names(pop.data)
  for(row.name in names(strata.map)){
    index <- if(length(region@strata.name) <= 1) seq_along(labels) else which(labels %in% strata.map[[row.name]])
    n.ind <- if(has.size) sum(pop.data$size[index]) else length(index)
    results$individuals$true.N[row.name, "N", i] <- n.ind
    if(!is.null(results$clusters$true.N)){
      results$clusters$true.N[row.name, "N", i] <- length(index)
    }
  }
  results
}

store.truth <- function(results, truth, i){
  # Stores the truth returned by a population generator for repetition i
  if(is.null(truth) || is.null(results$truth)){
    return(results)
  }
  results$truth[1, c("N", "D"), i] <- truth[c("N", "D")]
  results
}

count.in.truncation <- function(dist.data, left, right){
  # Number of detections with distances within the truncation distances
  distances <- dist.data$distance
  distances <- distances[!is.na(distances)]
  if(length(distances) == 0 || is.na(right)){
    return(0)
  }
  sum(distances >= left & distances <= right)
}

store.zero.detection.results <- function(results, transects, region, group.strata, left, right, i){
  # Records a repetition with no detections within the truncation distance as
  # an estimate of zero (zero.n = "zero")
  samplers <- transects@samplers
  is.line <- inherits(transects, "Line.Transect")
  if(is.line){
    effort <- as.numeric(sf::st_length(samplers))
  }else{
    effort <- rep(1, nrow(samplers))
  }
  sampler.strata <- as.character(samplers$strata)
  strata.map <- results.strata.map(dimnames(results$individuals$summary)[[1]],
                                   region, group.strata)
  region.strata <- region@strata.name
  if(length(region.strata) == 0){
    region.strata <- region@region.name
  }
  width <- right - left
  for(row.name in names(strata.map)){
    if(length(region@strata.name) <= 1){
      design.strata <- region.strata
      samp.index <- seq_along(sampler.strata)
    }else{
      design.strata <- strata.map[[row.name]]
      samp.index <- which(sampler.strata %in% design.strata)
    }
    area <- sum(region@area[region.strata %in% design.strata])
    row.effort <- sum(effort[samp.index])
    covered <- if(is.line) 2 * width * row.effort else pi * (right^2 - left^2) * row.effort
    summary.values <- c(Area = area, CoveredArea = covered, Effort = row.effort,
                        n = 0, n.miss.dist = 0, k = length(samp.index),
                        ER = 0, se.ER = NA, cv.ER = NA)
    estimate.values <- c(Estimate = 0, se = NA, cv = NA, lcl = 0, ucl = NA, df = NA)
    results$individuals$summary[row.name, names(summary.values), i] <- summary.values
    results$individuals$N[row.name, names(estimate.values), i] <- estimate.values
    results$individuals$D[row.name, names(estimate.values), i] <- estimate.values
    if(!is.null(results$clusters)){
      results$clusters$summary[row.name, names(summary.values), i] <- summary.values
      results$clusters$N[row.name, names(estimate.values), i] <- estimate.values
      results$clusters$D[row.name, names(estimate.values), i] <- estimate.values
    }
  }
  results$Detection[1, , i] <- NA
  results$Detection[1, "SuccessfulModels", i] <- 0
  if("ZeroDetections" %in% dimnames(results$Detection)[[2]]){
    results$Detection[1, "ZeroDetections", i] <- 1
  }
  results
}

get.rep.index <- function(detection, reps, model.count, use.max.reps){
  # Returns the index of the repetitions to include in the summaries: those
  # where all (or with use.max.reps at least one) models were fitted, plus
  # those recorded as zero detections (zero.n = "zero")
  if(!("SuccessfulModels" %in% dimnames(detection)[[2]])){
    return(seq_len(reps))
  }
  successful <- detection[1, "SuccessfulModels", seq_len(reps)]
  if(use.max.reps){
    include <- successful > 0
  }else{
    include <- successful == model.count
  }
  if("ZeroDetections" %in% dimnames(detection)[[2]]){
    zero <- detection[1, "ZeroDetections", seq_len(reps)]
    include <- include | (!is.na(zero) & zero == 1)
  }
  which(include)
}
