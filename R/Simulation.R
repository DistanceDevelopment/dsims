#' @include Detectability.R
#' @include Population.Description.R
#' @include generic.functions.R

#' @title Class "Simulation"
#'
#' @description Class \code{"Simulation"} is an S4 class containing descriptions of the
#' region, population, survey design and analyses the user wishes to investigate.
#' Once the simulation has been run the N.D.Estimates will contain multiple
#' estimates of abundance and density obtained by repeatedly generating
#' populations, simulating the survey and completing the analyses.
#'
#' @name Simulation-class
#' @title S4 Class "Simulation"
#' @slot reps Object of class \code{"numeric"}; the number of
#'  times the simulation should be repeated.
#' @slot single.transect.set Object of class \code{"logical"}; if
#'  \code{TRUE} the same set of transects are used in each repetition.
#' @slot design Object of class \code{"Survey.Design"}; the
#'  survey design.
#' @slot population.description Object of class \code{"Population.Description"};
#' the population.description.
#' @slot detectability Object of class \code{"Detectability"}; a
#'  description of the detectability of the population.
#' @slot ds.analysis Object of class \code{"DS.Analysis"}
#' @slot add.options a list to expand simulation options in the future.
#' @slot ddf.param.ests Object of class \code{"array"}; stores the
#'  parameters associated with the detection function.
#' @slot results A \code{"list"} of \code{"arrays"}; stores
#'  the estimated of abundance and density as well as other measures
#'  of interest.
#' @slot warnings A \code{"list"} to store warnings and error messages encountered
#'  during runtime.
#' @section Methods:
#' \describe{
#'  \item{\code{summary}}{\code{signature=(object = "Simulation")}: produces
#'  a summary of the simulation and its results.}
#'  \item{\code{generate.population}}{\code{signature = (object =
#'  "Simulation")}: generates a single instance of a population.}
#'  \item{\code{generate.transects}}{\code{signature = (object =
#'  "Simulation")}: generates a single set of transects.}
#'  \item{\code{run.survey}}{\code{signature = (object =
#'  "Simulation")}: carries out the simulation process as far as generating
#'  the distance data and returns an object containing the population,
#'  transects and data.}
#'  \item{\code{run.simulation}}{\code{signature = (simulation = "Simulation")}: runs
#'  the whole simulation for the specified number of repetitions.}
#' }
#' @keywords classes
#' @rdname Simulation-class
#' @importClassesFrom dssd Region
#' @seealso \code{\link{make.simulation}}
setClass("Simulation", representation(reps = "numeric",
                                      design = "Survey.Design",
                                      population.description = "Population.Description",
                                      detectability = "Detectability",
                                      ds.analysis = "DS.Analysis",
                                      add.options = "list",
                                      ddf.param.ests = "array",
                                      results = "list",
                                      warnings = "list"))

#' @importFrom methods validObject
#' @importFrom dssd make.design
setMethod(
  f="initialize",
  signature="Simulation",
  definition=function(.Object, reps = 10, single.transect.set = FALSE, design = make.design(), population.description = make.population.description(), detectability = make.detectability(), ds.analysis = make.ds.analysis(), results = list()){
    #Set slots
    .Object@reps            <- reps
    .Object@design          <- design
    .Object@population.description <- population.description
    .Object@detectability   <- detectability
    .Object@ds.analysis     <- ds.analysis
    .Object@results         <- results
    .Object@warnings        <- list()
    #Check object is valid
    valid <- validObject(.Object, test = TRUE)
    if(class(valid) == "character"){
      stop(paste(valid), call. = FALSE)
    }
    # return object
    return(.Object)
  }
)

setValidity("Simulation",
            function(object){
              strata.names <- object@design@region@strata.name
              # truncation
              # Check to see if the analysis truncation distance is larger than the
              # if(length(object@ds.analysis@truncation[[1]]) > 0){
              #   if(object@ds.analysis@truncation > object@detectability@truncation ||
              #      object@ds.analysis@truncation > object@design@truncation){
              #     warning("The truncation distance for analysis is larger than the truncation distance for data generation, this will likely cause biased results.", immediate. = TRUE, call. = FALSE)
              #   }
              # }
              # Population.Description checks
              pop.desc <- object@population.description


              # Detectability checks

              # Check number of scale parameters is equal to

              # Analysis checks
              if(object@ds.analysis@er.var != "R2" && inherits(object@design, "Line.Transect.Design")){
                if(!all(object@design@design == "systematic")){
                  return("Variance estimators other than R2 are currently only supported for systematic parallel line designs.")
                }
              }

              return(TRUE)
            }
)

# GENERIC METHODS DEFINITIONS --------------------------------------------

#' @rdname generate.population-methods
#' @export
setMethod(
  f="generate.population",
  signature="Simulation",
  definition=function(object, ...){
    population <- generate.population(object = object@population.description,
                                      detectability = object@detectability,
                                      region = object@design@region)
    return(population)
  }
)

#' generate.transects
#'
#' Generates a set of transects based on the design provided.
#'
#' @param object object of class Simulation
#' @param quiet if TRUE silences some warnings
#' @param ... not implemented
#' @return an object of class Transect from dssd package
#' @rdname generate.transects.Simulation-methods
#' @importFrom stats na.omit qlnorm qnorm
#' @importFrom methods slotNames
#' @importFrom methods new
#' @export
setMethod(
  f="generate.transects",
  signature="Simulation",
  definition=function(object, quiet = FALSE, ...){
    transect <- generate.transects(object = object@design,
                                   region = object@design@region)
    return(transect)
  }
)

#' @rdname run.survey-methods
#' @export
#' @importFrom methods new
setMethod(
  f="run.survey",
  signature="Simulation",
  definition=function(object){
    # Create the population and transects for the survey
    population <- generate.population(object)
    transects <- generate.transects(object)
    if(class(transects) %in% c("Line.Transect", "Segment.Transect")){
      # Check transects for empty geometries
      transects <- check.transects(transects)
      survey <- new(Class = "Survey.LT",
                    population = population,
                    transect = transects,
                    perp.truncation = object@design@truncation)
    }else if(class(transects) == "Point.Transect"){
      survey <- new(Class = "Survey.PT",
                    population = population,
                    transect = transects,
                    rad.truncation = object@design@truncation)
    }
    # Simulate the survey
    survey <- run.survey(survey, region = object@design@region)
    return(survey)
  }
)


#' show
#'
#' Not currently implemented
#'
#' @param object object of class Simulation
#' @rdname show.Simulation-methods
#' @return No return value, displays a summary of the simulation
#' @export
setMethod(
  f="show",
  signature="Simulation",
  definition=function(object){
    summary <- summary(object, description.summary = FALSE)
    show(summary)
  }
)

#' histogram.N.ests
#'
#' Plots a histogram of the estimates abundances
#'
#' @param x object of class Simulation
#' @param ... optional parameters to pass to the generic hist function in graphics
#' @rdname histogram.N.ests-methods
#' @return No return value, displays a histogram of the abundance estimates
#' @importFrom graphics hist abline
#' @export
histogram.N.ests <- function(x, ...){
  reps <- x@reps
  sum.sim <- summary(x, description.summary = FALSE)
  if(sum.sim@failures == reps){
    warning("None of the simulation repetitions were successful, cannot plot histogram of estimates.", immediate. = TRUE, call. = TRUE)
  }else{
    index <- dim(x@results$individuals$N)[1]
    true.N <- sum(x@population.description@N)
    if(!is.null(x@results$clusters)){
      ests <- x@results$clusters$N[index, "Estimate", 1:reps]
      hist(ests, main = "Histogram of Estimates", xlab = "Estimated Abundance of Clusters", ...)
    }else{
      ests <- x@results$individuals$N[index, "Estimate", 1:reps]
      hist(ests, main = "Histogram of Estimates", xlab = "Estimated Abundance of Individuals", ...)
    }
    abline(v = true.N, col = 2, lwd = 3, lty = 2)
  }
  invisible(x)
}



#' summary
#'
#' Provides a summary of the simulation results.
#'
#' @param object object of class Simulation
#' @param description.summary logical indicating whether an
#'  explanation of the summary should be displayed
#' @param ... can specify if you want the maximum number of iterations to be used where at least one model converged (use.max.reps = TRUE) or only use iterations where all models converged (use.max.reps = FALSE)
#' @rdname summary.Simulation-methods
#' @importFrom stats na.omit qlnorm qnorm
#' @importFrom methods slotNames slot show
#' @return Object of class Simulation.Summary
#' @export
setMethod(
  f="summary",
  signature="Simulation",
  definition=function(object, description.summary = TRUE, ...){
    if(description.summary){
      description.summary()
    }
    #Get number of reps
    reps <- object@reps
    #Check for additional arguments
    args <- list(...)
    if (!("use.max.reps" %in% names(args))){
      #By default exclude all iterations where one or more models failed to converge
      use.max.reps = FALSE
    }else{
      use.max.reps = args$use.max.reps
    }
    #Get index of iterations to use
    model.count <- length(object@ds.analysis@dfmodel)
    #These will use max iters by default
    results <- object@results
    # Update summary stats depending on use.max.reps option
    if(use.max.reps){
      rep.index <- which(object@results$Detection[1,"SuccessfulModels",1:reps] > 0)
    }else{
      rep.index <- which(object@results$Detection[1,"SuccessfulModels",1:reps] == model.count)
      results <- add.summary.results(results = results,
                                     model.count = model.count,
                                     use.max.reps = use.max.reps)
    }

    #Create function to calculate RMSE
    calc.RMSE <- function(x, reps){
      true.x <- x[(reps+1)]
      x <- na.omit(x[1:reps])
      reps.success <- length(x)
      return( sqrt( sum((x-true.x)^2) / reps.success ))
    }
    #Calculate true values
    strata.names <- object@design@region@strata.name
    strata.order <- NULL
    #Deal with any grouping of strata
    analysis.strata <- object@ds.analysis@group.strata
    # Get density summary
    density.summary <- summary(object@population.description@density)@summary
    if(nrow(analysis.strata) > 0){
      #get strata names
      sub.strata.names <- strata.names
      strata.names <- unique(analysis.strata$analysis.id)
      #sum areas of sub strata
      areas <- N <- rep(NA, length(strata.names))
      for(strat in seq(along = strata.names)){
        #Get sub strata names
        sub.strata <- analysis.strata$design.id[which(analysis.strata$analysis.id == strata.names[strat])]
        #Find their index
        index <- which(sub.strata.names %in% sub.strata)
        areas[strat] <- sum(object@design@region@area[index])
        # Get abundance for strata
        if(object@population.description@gen.by.N){
          N[strat] <- object@population.description@N[index]
        }else{
          index2 <- which(density.summary$strata %in% sub.strata)
          N[strat] <- sum(density.summary$ave.N[index2])
        }
      }
      #Add on totals
      areas <- c(areas, sum(areas))
      N <- c(N, sum(N))
      #Otherwise process areas and Population size as normal
    }else{
      #Re ordering in the same way as the results tables (think dht arranges them)
      for(strat in seq(along = strata.names)){
        strata.order <- c(strata.order, which(strata.names == dimnames(results$individuals$N)[[1]][strat]))
      }
      if(object@population.description@gen.by.N){
        N <- object@population.description@N
      }else{
        N <- density.summary$ave.N
      }
      if(length(strata.names) > 1){
        N <- N[strata.order]
        N <- c(N, sum(N))
        areas <- c(object@design@region@area[strata.order], sum(object@design@region@area))
        strata.names <- c(strata.names[strata.order], "Pooled")
      }else{
        areas <- object@design@region@area
      }
    }
    if(is.null(results$clusters)){
      #If there are no clusters
      true.N.individuals <- N
      true.D.individuals <- true.N.individuals/areas
      expected.size <- data.frame()
    }else{
      #If there are clusters
      true.N.clusters <- N
      #calculate expected cluster size
      true.expected.s <- numeric()
      size.list <- object@population.description@covariates[["size"]]
      for(i in seq(along = size.list)){
        if(class(size.list[[i]]) == "data.frame"){
          true.expected.s[i] <- sum(size.list[[i]]$level*size.list[[i]]$prob)
        }else{
          size.dist <- size.list[[i]]
          dist <- size.dist[[1]]
          dist.param <- size.dist[[2]]
          true.expected.s[i] <- switch(dist,
                                       "normal" = dist.param$mean,
                                       "poisson" = dist.param$lambda,
                                       "ztruncpois" = dist.param$mean,
                                       "lognormal" = exp(dist.param$meanlog + 0.5 * dist.param$sdlog^2))
        }
      }
      # Re-order to match
      true.expected.s <- true.expected.s[strata.order]
      # Add average
      if(length(size.list) > 1){
        true.expected.s <- c(true.expected.s, sum(true.N.clusters[1:length(true.expected.s)]*true.expected.s)/sum(true.N.clusters[1:length(true.expected.s)]))
      }
      #calculate expected number of individuals
      true.N.individuals <- true.N.clusters*true.expected.s
      true.D.individuals <- true.N.individuals/areas
      true.D.clusters <- true.N.clusters/areas
    }

    #Create summary tables
    included.reps <- length(rep.index)
    capture <- array(NA, dim = c(included.reps, length(true.N.individuals)))
    capture.D <- array(NA, dim = c(included.reps, length(true.D.individuals)))
    zero.n <- array(NA, dim = c(included.reps, length(true.N.individuals)))
    for(strat in seq(along = true.N.individuals)){
      for(i in seq(along = rep.index)){
        capture[i, strat] <- ifelse(results$individuals$N[strat, "lcl", i] < true.N.individuals[strat] & results$individuals$N[strat, "ucl", i] > true.N.individuals[strat], TRUE, FALSE)
        capture.D[i, strat] <- ifelse(results$individuals$D[strat, "lcl", i] < true.D.individuals[strat] & results$individuals$D[strat, "ucl", i] > true.D.individuals[strat], TRUE, FALSE)
        zero.n[i, strat] <- ifelse(results$individuals$summary[strat, "n", i] == 0, TRUE, FALSE)
      }
    }
    #Calculates the percentage of times the true value is whithin the confidence intervals
    percent.capture <- (apply(capture, 2, sum, na.rm = TRUE)/nrow(na.omit(capture)))*100
    percent.capture.D <- (apply(capture.D, 2, sum, na.rm = TRUE)/nrow(na.omit(capture)))*100
    zero.n <- apply(zero.n, 2, sum, na.rm = TRUE)
    if(length(true.N.individuals) == 1){
      RMSE.N = apply(cbind(t(as.matrix(results$individuals$N[, "Estimate", rep.index])), true.N.individuals), 1, calc.RMSE, reps = length(rep.index))
      RMSE.D = apply(cbind(t(as.matrix(results$individuals$D[, "Estimate", rep.index])), true.D.individuals), 1, calc.RMSE, reps = length(rep.index))
    }else{
      RMSE.N = apply(cbind(results$individuals$N[, "Estimate", rep.index], true.N.individuals), 1, calc.RMSE, reps = length(rep.index))
      RMSE.D = apply(cbind(results$individuals$D[, "Estimate", rep.index], true.D.individuals), 1, calc.RMSE, reps = length(rep.index))
    }
    individual.summary <- data.frame(mean.Cover.Area = results$individuals$summary[,"CoveredArea","mean"],
                                     mean.Effort = results$individuals$summary[,"Effort","mean"],
                                     mean.n = results$individuals$summary[,"n","mean"],
                                     mean.n.miss.dist = ifelse("n.miss.dist" %in% dimnames(results$individuals$summary)[[2]], results$individuals$summary[,"n.miss.dist","mean"], NA),
                                     no.zero.n = zero.n,
                                     mean.ER = results$individuals$summary[,"ER","mean"],
                                     mean.se.ER = results$individuals$summary[,"se.ER","mean"],
                                     sd.mean.ER = results$individuals$summary[,"ER","sd"])
    # Remove unnecessary columns
    if(all(is.na(individual.summary$mean.n.miss.dist)) || all(individual.summary$mean.n.miss.dist == 0)){
      # To keep CRAN check happy!
      eval(parse(text = paste("individual.summary <- subset(individual.summary, select = -mean.n.miss.dist)")))
    }
    if(all(individual.summary$no.zero.n == 0)){
      eval(parse(text = paste("individual.summary <- subset(individual.summary, select = -no.zero.n)")))
    }
    individual.N <- data.frame(Truth = true.N.individuals,
                               mean.Estimate = results$individuals$N[,"Estimate","mean"],
                               percent.bias = (results$individuals$N[,"Estimate","mean"] - true.N.individuals)/true.N.individuals*100,
                               RMSE = RMSE.N,
                               CI.coverage.prob = percent.capture/100,
                               mean.se = results$individuals$N[,"se","mean"],
                               sd.of.means = results$individuals$N[,"Estimate","sd"])
    individual.D <- data.frame(Truth = true.D.individuals,
                               mean.Estimate = results$individuals$D[,"Estimate","mean"],
                               percent.bias = (results$individuals$D[,"Estimate","mean"] - true.D.individuals)/true.D.individuals*100,
                               RMSE = RMSE.D,
                               CI.coverage.prob = percent.capture.D/100,
                               mean.se = results$individuals$D[,"se","mean"],
                               sd.of.means = results$individuals$D[,"Estimate","sd"])

    if(!is.null(results$clusters)){
      capture <- array(NA, dim = c(included.reps, length(true.N.clusters)))
      capture.D <- array(NA, dim = c(included.reps, length(true.D.clusters)))
      zero.n <- array(NA, dim = c(included.reps, length(true.N.clusters)))
      for(strat in seq(along = true.N.clusters)){
        for(i in seq(along = rep.index)){
          capture[i, strat] <- ifelse(results$clusters$N[strat, "lcl", i] < true.N.clusters[strat] & results$clusters$N[strat, "ucl", i] > true.N.clusters[strat], TRUE, FALSE)
          capture.D[i, strat] <- ifelse(results$clusters$D[strat, "lcl", i] < true.D.clusters[strat] & results$clusters$D[strat, "ucl", i] > true.D.clusters[strat], TRUE, FALSE)
          zero.n[i, strat] <- ifelse(results$clusters$summary[strat, "n", i] == 0, TRUE, FALSE)
        }
      }
      percent.capture <- (apply(capture, 2, sum, na.rm = TRUE)/nrow(na.omit(capture)))*100
      percent.capture.D <- (apply(capture.D, 2, sum, na.rm = TRUE)/nrow(na.omit(capture.D)))*100
      zero.n <- apply(zero.n, 2, sum, na.rm = TRUE)
      if(length(true.N.clusters) == 1){
        RMSE.N = apply(cbind(t(as.matrix(results$clusters$N[, "Estimate", rep.index])), true.N.clusters), 1, calc.RMSE, reps = length(rep.index))
        RMSE.D = apply(cbind(t(as.matrix(results$clusters$D[, "Estimate", rep.index])), true.D.clusters), 1, calc.RMSE, reps = length(rep.index))
      }else{
        RMSE.N = apply(cbind(results$clusters$N[, "Estimate", rep.index], true.N.clusters), 1, calc.RMSE, reps = length(rep.index))
        RMSE.D = apply(cbind(results$clusters$D[, "Estimate", rep.index], true.D.clusters), 1, calc.RMSE, reps = length(rep.index))
      }
      cluster.summary <- data.frame(mean.Cover.Area = results$clusters$summary[,"CoveredArea","mean"],
                                    mean.Effort = results$clusters$summary[,"Effort","mean"],
                                    mean.n = results$clusters$summary[,"n","mean"],
                                    mean.n.miss.dist = ifelse("n.miss.dist" %in% dimnames(results$clusters$summary)[[2]], results$clusters$summary[,"n.miss.dist","mean"], NA),
                                    no.zero.n = zero.n,
                                    mean.k = results$clusters$summary[,"k","mean"],
                                    mean.ER = results$clusters$summary[,"ER","mean"],
                                    mean.se.ER = results$clusters$summary[,"se.ER","mean"],
                                    sd.mean.ER = results$clusters$summary[,"ER","sd"])
      # Remove unnecessary columns
      if(all(is.na(cluster.summary$mean.n.miss.dist)) || all(cluster.summary$mean.n.miss.dist == 0)){
        eval(parse(text = paste("cluster.summary <- subset(cluster.summary,select = -mean.n.miss.dist)")))
      }
      if(all(cluster.summary$no.zero.n == 0)){
        eval(parse(text = paste("cluster.summary <- subset(cluster.summary,select = -no.zero.n)")))
      }
      cluster.N <- data.frame(Truth = true.N.clusters,
                              mean.Estimate = results$clusters$N[,"Estimate","mean"],
                              percent.bias = (results$clusters$N[,"Estimate","mean"] - true.N.clusters)/true.N.clusters*100,
                              RMSE = RMSE.N,
                              #lcl = results$clusters$N[,"lcl","mean"],
                              #ucl = results$clusters$N[,"ucl","mean"],
                              CI.coverage.prob = percent.capture/100,
                              mean.se = results$clusters$N[,"se","mean"],
                              sd.of.means = results$clusters$N[,"Estimate","sd"])
      cluster.D <- data.frame(Truth = true.D.clusters,
                              mean.Estimate = results$clusters$D[,"Estimate","mean"],
                              percent.bias = (results$clusters$D[,"Estimate","mean"] - true.D.clusters)/true.D.clusters*100,
                              RMSE = RMSE.D,
                              #lcl = results$clusters$N[,"lcl","mean"],
                              #ucl = results$clusters$N[,"ucl","mean"],
                              CI.coverage.prob = percent.capture.D/100,
                              mean.se = results$clusters$D[,"se","mean"],
                              sd.of.means = results$clusters$D[,"Estimate","sd"])
      expected.size <- data.frame(Truth = true.expected.s,
                                  mean.Expected.S = results$expected.size[,"Expected.S","mean"],
                                  percent.bias = abs(true.expected.s - results$expected.size[,"Expected.S","mean"])/true.expected.s*100,
                                  mean.se.ExpS = results$expected.size[,"se.Expected.S","mean"],
                                  sd.mean.ExpS = results$expected.size[,"Expected.S","sd"])
      clusters <- list(summary = cluster.summary, N = cluster.N, D = cluster.D)
    }
    else{
      clusters <- list()
    }

    # Detection function
    detection <- data.frame(mean.observed.Pa = results$Detection[,"True.Pa","mean"],
                            mean.estimate.Pa = results$Detection[,"Pa","mean"],
                            sd.estimate.Pa = results$Detection[,"Pa","sd"],
                            mean.ESW = results$Detection[,"ESW","mean"],
                            sd.ESW = results$Detection[,"ESW","sd"])
    #Find how many iterations failed
    no.fails <- reps - included.reps
    individuals <- list(summary = individual.summary, N = individual.N, D = individual.D)
    #Model selection table
    tab.model.selection <- table(results$Detection[,"SelectedModel",rep.index])
    #Create detectabilty summary
    detectability.summary <- list(key.function = object@detectability@key.function,
                                  scale.param = object@detectability@scale.param,
                                  shape.param = object@detectability@shape.param,
                                  cov.param = object@detectability@cov.param,
                                  truncation = object@detectability@truncation)

    #Create design summary
    design.type = character()
    for(i in seq(along = object@design@design)){
      if(class(object@design) %in% c("Line.Transect.Design", "Segment.Transect.Design")){
        design.type[i] <- switch(object@design@design[i],
                              systematic = "Systematic parallel line design",
                              eszigzag = "Equal spaced zigzag line design",
                              eszigzagcom = "Equal spaced zigzag with complementary lines design",
                              random = "Random parallel line design",
                              segmentedgrid = "Segmented line transect grid design")
      }else{
        design.type[i] <- switch(object@design@design[i],
                              systematic = "Systematic point design",
                              random = "Random sampling point design")

      }
    }
    slots <- slotNames(object@design)
    design.parameters <- list()
    design.parameters$design.type <- design.type
    for(i in seq(along = slots)){
      if(slots[i] %in% c("design.angle", "spacing", "edge.protocol", "nested.space",
                         "line.length", "bounding.shape", "samplers", "effort.allocation",
                         "truncation")){
        if(!(length(slot(object@design, slots[i])) == 0)){
          design.parameters[[slots[i]]] <- slot(object@design, slots[i])
        }
      }
    }

    #Create analysis summary
    analysis.summary <- list(dfmodels = object@ds.analysis@dfmodel,
                             key = object@ds.analysis@key,
                             criteria = object@ds.analysis@criteria,
                             variance.estimator = object@ds.analysis@er.var,
                             truncation = object@ds.analysis@truncation)
    if(length(object@ds.analysis@cutpoints) > 0){
      analysis.summary$cutpoints <- object@ds.analysis@cutpoints
    }
    if(nrow(analysis.strata) > 0){
      analysis.summary$analysis.strata <- object@ds.analysis@group.strata
    }
    if(length(object@ds.analysis@adjustment) > 0){
      analysis.summary$adjustment <- object@ds.analysis@adjustment
    }
    if(length(object@ds.analysis@control.opts) > 0){
      analysis.summary$control <- object@ds.analysis@control.opts
    }

    #Create population summary
    pop.covars <- object@population.description@covariates

    #Create simulation summary object
    summary.x <- new(Class = "Simulation.Summary",
                     region.name = object@design@region@region.name,
                     strata.name = object@design@region@strata.name,
                     total.reps = object@reps,
                     failures = no.fails,
                     use.max.reps = use.max.reps,
                     individuals = individuals,
                     clusters = clusters,
                     expected.size = expected.size,
                     population.covars = pop.covars,
                     detection = detection,
                     model.selection = tab.model.selection,
                     design.summary = design.parameters,
                     detectability.summary = detectability.summary,
                     analysis.summary = analysis.summary)

    return(summary.x)
  }
)



