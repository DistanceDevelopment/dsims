#' @include Simulation.R
#' @include Population.Description.R
#' @include Detectability.R
#' @include Density.R
#' @include DS.Analysis.R

#' @title Creates a Density object
#' @description Creates a density grid across the study area describing the distribution
#' of animals.
#' @details
#' There are multiple ways to create the density grid. The most straight forward
#' is to create a grid with constant values (to which high and low areas can later
#' be added) or pass in a fitted \code{mgcv gam}. The gam model should only be fitted
#' with x and y as explanatory variables. If you plan on trying multiple
#' animal distributions by adding high and low areas to a constant surface it is
#' recommended to make a copy of the initial flat density grid object as the first
#' step in grid generation is computationally intensive and can take a little while
#' to complete, especially if you have a fine density grid.
#' @param region the Region object in which the density grid will be created
#' @param x.space the intervals in the grid in the x direction
#' @param y.space the intervals in the grid in the y direction
#' @param constant a value describing a constant density across the surface. If not supplied a default value of 1 is used for all strata.
#' @param fitted.model \code{gam} object created using \code{mgcv} with only x and y as explanatory covariates.
#' @param density.formula a formula of x and/or y describing the
#' density surface.
#' @param density.surface Object of class \code{list}; an sf grid recording
#' the density grid polygons, density values within those polygons and the
#' central x and y coordinates.
#' @return \code{\link{Density-class}} object
#' @export
#' @importFrom dssd make.region
#' @importFrom methods new
#' @author Laura Marshall
#' @seealso \code{\link{make.region}}
#' @examples
#' # A simple density surface with a constant value of 1 can be created within a rectangular
#' # Create a region from shapefile
#' shapefile.name <- system.file("extdata", "StAndrew.shp", package = "dssd")
#' region <- make.region(region.name = "St Andrews bay",
#'                       shape = shapefile.name)
#'
#' # Create a density object
#' density <- make.density(region = region,
#'                        x.space = 1000,
#'                        constant = 1)
#'
#' # Add some ares of higher / lower density
#' density <- add.hotspot(object = density,
#'                        centre = c(-170000, 6255000),
#'                        sigma = 10000,
#'                        amplitude = 4)
#' density <- add.hotspot(object = density,
#'                        centre = c(-150000, 6240000),
#'                        sigma = 10000,
#'                        amplitude = -0.9)
#'
#' # Plot the density
#' plot(density, region)
#'
make.density <- function(region = make.region(), x.space = 20, y.space = NULL, constant = numeric(0), fitted.model = NULL, density.formula = NULL, density.surface = list()){
  # Check if the user has supplied a y.space value
  if(is.null(y.space)){
    # If not set it equal to x.space
    y.space <- x.space
  }
  # Check how many grid points there will likely be
  est.grid.points <- (sum(region@area)/(x.space)^2)*(x.space/y.space)
  if(est.grid.points < 25){
    stop("Your grid spacing will result in less than 25 grid cells. Please reduce your x.space (and optionally your y.space) argument.", call. = FALSE)
  }else if(est.grid.points > 50000){
    stop("Your grid spacing will result in more than 50,000 grid cells. Please increase your x.space (and optionally your y.space) argument.", call. = FALSE)
  }else if(est.grid.points > 30000){
    warning("Your grid spacing arguments x.space (and y.space) will result in more than 30,000 grid cells being generated. You might have time to grab a coffee!", call. = FALSE, immediate. = TRUE)
  }else if(est.grid.points > 10000){
    warning("Your grid spacing arguments x.space (and y.space) will result in more than 10,000 grid cells being generated. This could take a moment!", call. = FALSE, immediate. = TRUE)
  }
  # Find the number of strata
  no.strata <- length(region@strata.name)
  # Check the user has supplied the correct number of consants
  if(length(constant) > 0){
    if(no.strata > 0 & length(constant) != length(region@strata.name)){
      if(length(constant) == 1){
        constant <- rep(constant, length(region@strata.name))
      }else{
        stop("The length of the constant vector does not correspond to the number of strata", call. = FALSE)
      }
    }
  }else{
    if(no.strata == 0){
      constant <- 1
    }else{
      constant <- rep(1, no.strata)
    }
  }
  # Make density object
  density <- new(Class = "Density", region = region, strata.name = region@strata.name, density.surface = density.surface, x.space = x.space, y.space = y.space, constant = constant, model.fit = fitted.model, density.formula)
  return(density)
}

#' @title Creates a Population.Description object
#'
#' @description
#' Creates an object which describes a population. The values in this object
#' will be used to create instances of the population.
#'
#' @details Individual-level covariate values can be defined as one of the following distributions: 'normal', 'poisson', 'ztruncpois' or 'lognormal'. The distribution name and the associated parameters as defined in the table below must be provided in a named list. Either one list can be provided for the entire study area or multiple lists grouped together as a list with one per strata.
#'
#' \tabular{lll}{ Distribution  \tab Parameters  \tab         \cr
#'                normal        \tab mean        \tab sd      \cr
#'                poisson       \tab lambda      \tab         \cr
#'                ztruncpois    \tab mean        \tab         \cr
#'                lognormal     \tab meanlog     \tab sdlog   \cr
#'               }
#'
#' @param region the Region object in which this population exists (see \link{make.region}).
#' @param density the Density object describing the distribution of the individuals / clusters (see \link{make.density}).
#' @param covariates Named list with one named entry per individual-level covariate. Cluster sizes can be defined here, it must be named 'size'. The distribution of covariate values can either be defined by specifying a particular distribution and its parameters or as a discrete distribution in a dataframe. Dataframes should have columns level and prob (and optionally strata) specifying the covariates levels, probabilities and strata if they are strata specific. Distributions can be defined as lists with named entries distribution and the relevant parameters as specified in details. A list of distributions can be provided with one for each strata.
#' @param N the number of individuals / clusters in a population with one value per
#' strata. Total population size is 1000 by default.
#' @param fixed.N a logical value. If TRUE the population is generated from the value(s)
#'  of N otherwise it is generated from the values in the density grid.
#' @return \code{\link{Population.Description-class}}
#' @export
#' @importFrom methods new
#' @importFrom dssd make.region
#' @author Laura Marshall
#' @seealso \code{\link{make.simulation}} \code{\link{make.detectability}} \code{\link{make.density}}
#' @examples
#' # Create a basic rectangular study area
#' region <- make.region()
#'
#' # Make a density grid (large spacing for speed)
#' density <- make.density(region = region,
#'                         x.space = 200,
#'                         y.space = 100,
#'                         constant = 1)
#' density <- add.hotspot(density, centre = c(1000, 100), sigma = 250, amplitude = 10)
#'
#' # Define some covariate values for out population
#' covs <- list()
#' covs$size <- list(distribution = "ztruncpois", mean = 5)
#'
#' # Define the population description
#' popdsc <- make.population.description(region = region,
#'                                       density = density,
#'                                       covariates = covs,
#'                                       N = 200)
#'
#' # define the detecability
#' detect <- make.detectability(key.function = "hn", scale.param = 25, truncation = 50)
#'
#' # generate an example population
#' pop <- generate.population(popdsc, region = region, detectability = detect)
#'
#' plot(pop, region)
#'
#' # Multi-strata example (make sf shape)
#' s1 = matrix(c(0,0,0,2,1,2,1,0,0,0),ncol=2, byrow=TRUE)
#' s2 = matrix(c(1,0,1,2,2,2,2,0,1,0),ncol=2, byrow=TRUE)
#' pol1 = sf::st_polygon(list(s1))
#' pol2 = sf::st_polygon(list(s2))
#' sfc <- sf::st_sfc(pol1,pol2)
#' strata.names <- c("low", "high")
#' sf.pol <- sf::st_sf(strata = strata.names, geom = sfc)
#'
#' region <- make.region(region.name = "Multi-strata Eg",
#'                       strata.name = strata.names,
#'                       shape = sf.pol)
#' \donttest{
#' density <- make.density(region = region,
#'                         x.space = 0.22,
#'                         constant = c(10,80))
#'
#' covs <- list()
#' covs$size <- list(list(distribution = "poisson", lambda = 25),
#'                   list(distribution = "poisson", lambda = 15))
#' covs$sex <- data.frame(level = rep(c("male", "female"),2),
#'                       prob = c(0.5, 0.5, 0.6, 0.4),
#'                       strata = c(rep("low",2),rep("high",2)))
#'
#' # Define the population description (this time using the density to determine
#' # the population size)
#' popdesc <- make.population.description(region = region,
#'                                        density = density,
#'                                        covariates = covs,
#'                                        fixed.N = FALSE)
#'
#' # define the detecability (see make.detectability to alter detection function
#' # for different covariate values)
#' detect <- make.detectability(key.function = "hn", scale.param = 25, truncation = 50)
#'
#' # generate an example population
#' pop <- generate.population(popdesc, region = region, detectability = detect)
#'
#' plot(pop, region)
#' }
#'
make.population.description <- make.pop.description <- function(region = make.region(), density = make.density(), covariates = list(), N = numeric(0), fixed.N = TRUE){
  # Check all covariates are named
  if(any(names(covariates) == "")){
    stop("All elements in the covariate list must be names with the covariate name.", call. = FALSE)
  }
  # Pre-processing of covariates - check new form and convert to old form
  for(cov in seq(along = covariates)){
    # For single strata cases wrap cov option in additional list
    if("distribution" %in% names(covariates[[cov]]) || class(covariates[[cov]]) == "data.frame"){
      covariates[[cov]] <- list(covariates[[cov]])
    }
  }
  cov.names <- names(covariates)
  for(cov in seq(along = covariates)){
    strat.list <- list()
    for(i in seq(along = covariates[[cov]])){
      if(class(covariates[[cov]][[i]]) == "data.frame"){
        if(!all(c("level", "prob") %in% names(covariates[[cov]][[i]]))){
          stop("Covariate dataframes must contain the columns 'level' and 'prob'.", call. = FALSE)
        }
        if(length(covariates[[cov]]) > 1){
          stop("Please only supply one covariate dataframe with strata as a column for multi-strata covariate values.", call. = FALSE)
        }
        strat.names <- region@strata.name
        if("strata" %in% names(covariates[[cov]][[i]]) && length(covariates[[cov]]) == 1 && length(strat.names) > 1){
          strat.names.check <- unique(covariates[[cov]][[i]]$strata)
          if(any(sort(strat.names) != sort(strat.names.check))){
            stop(paste("The strata names in the covariate dataframe for ", cov.names[cov], " do not match the strata names in the region object.", sep = ""), call. = FALSE)
          }
          # Separte this table out into separate list elements
          for(j in seq(along = strat.names)){
            cov.dataframe <- covariates[[cov]][[i]]
            strat.list[[j]] <- cov.dataframe[cov.dataframe$strata == strat.names[j],c("level","prob")]
          }
        }
      }else if(class(covariates[[cov]][[i]]) == "list"){
        # Find what parameters should be supplied given the distribution
        params <- switch(covariates[[cov]][[i]]$distribution,
                         normal = c("mean", "sd"),
                         poisson = "lambda",
                         ztruncpois = "mean",
                         lognormal = c("meanlog", "sdlog"))
        if(!all(params %in% names(covariates[[cov]][[i]]))){
          stop(paste("You have not supplied all the required parameters (", paste(params, collapse = ", "),") for the following covariate distribution: ", covariates[[cov]][[i]]$distribution, sep = ""), call. = FALSE)
        }
        # Separate out into old format
        pvs <- covariates[[cov]][[i]]
        param.vals <- switch(covariates[[cov]][[i]]$distribution,
                             normal = list(mean = pvs$mean, sd = pvs$sd),
                             poisson = list(lambda = pvs$lambda),
                             ztruncpois = list(mean = pvs$mean),
                             lognormal = list(meanlog = pvs$meanlog, sdlog = pvs$sdlog))
        old.format <- list(covariates[[cov]][[i]]$distribution, param.vals)
        strat.list[[i]] <- old.format
      }
    }
    if(length(strat.list) > 0){
      covariates[[cov]] <- strat.list
    }
  }
  # Passes all arguments to function to make a new instance of the class
  pop.description <- new(Class = "Population.Description", N = N, density = density, region.obj = region, covariates = covariates, gen.by.N = fixed.N)
  return(pop.description)
}

#' @title Creates a Detectability object
#' @description
#' The detectability of the population is described by the values in this
#' class.
#'
#' @param key.function specifies shape of the detection function (either
#'   half-normal "hn", hazard rate "hr" or uniform "uf")
#' @param scale.param numeric vector with either a single value to be applied globally or a value for each strata. These should be supplied on the natural scale.
#' @param shape.param numeric vector with either a single value to be applied globally or a value for each strata. These should be supplied on the natural scale.
#' @param cov.param Named list with one named entry per individual level covariate. Covariate parameter values should be defined on the log scale (rather than the natural scale), this is the same scale as provided in the ddf output in mrds and also in the MCDS output in Distance. Cluster sizes parameter values can be defined here. Each list entry will either be a data.frame containing 2 or 3 columns: level, param and where desired strata. If the region has multiple strata but this column is omitted then the values will be assumed to apply globally. The cluster size entry in the list must be named 'size'. Alternatively the list element may a numeric vector with either a single value to be applied globally or a value for each strata.
#' @param truncation the maximum perpendicular (or radial) distance at which
#'   objects may be detected from a line (or point) transect.
#' @return \code{\link{Detectability-class}} object
#' @export
#' @seealso \code{\link{make.simulation}} \code{\link{make.population.description}} \code{\link{make.density}}
#' @importFrom methods new
#' @author Laura Marshall
#' @examples
#' # Multi-strata example (make sf shape)
#' s1 = matrix(c(0,0,0,2,1,2,1,0,0,0),ncol=2, byrow=TRUE)
#' s2 = matrix(c(1,0,1,2,2,2,2,0,1,0),ncol=2, byrow=TRUE)
#' pol1 = sf::st_polygon(list(s1))
#' pol2 = sf::st_polygon(list(s2))
#' sfc <- sf::st_sfc(pol1,pol2)
#' strata.names <- c("low", "high")
#' sf.pol <- sf::st_sf(strata = strata.names, geom = sfc)
#'
#' region <- make.region(region.name = "Multi-strata Eg",
#'                       strata.name = strata.names,
#'                       shape = sf.pol)
#'
#' density <- make.density(region = region,
#'                         x.space = 0.22,
#'                         constant = c(20,50))
#'
#' covs <- list()
#' covs$size <- list(list(distribution = "poisson", lambda = 25),
#'                   list(distribution = "poisson", lambda = 15))
#' covs$sex <- data.frame(level = rep(c("male", "female"),2),
#'                       prob = c(0.5, 0.5, 0.6, 0.4),
#'                       strata = c(rep("low",2),rep("high",2)))
#'
#' # Define the population description (this time using the density to determine
#' # the population size)
#' popdesc <- make.population.description(region = region,
#'                                        density = density,
#'                                        covariates = covs,
#'                                        fixed.N = FALSE)
#'
#' cov.param <- list()
#' cov.param$size <- c(log(1.02),log(1.005))
#' cov.param$sex <- data.frame(level = c("male", "female", "male", "female"),
#'                             param = c(log(1.5), 0, log(1.7), log(1.2)),
#'                             strata = c("low","low","high","high"))
#'
#' # define the detecability
#' detect <- make.detectability(key.function = "hn",
#'                              scale.param = 0.08,
#'                              cov.param = cov.param,
#'                              truncation = 0.2)
#'
#' plot(detect, popdesc)
#'
make.detectability <- function(key.function = "hn", scale.param = 25, shape.param = numeric(0), cov.param = list(), truncation = 50){
  # Passes all arguments to function to make a new instance of the class
  detectability <- new(Class = "Detectability", key.function = key.function, scale.param = scale.param, shape.param = shape.param, cov.param = cov.param, truncation = truncation)
  return(detectability)
}

#' @title Creates an Analysis object
#' @description
#' This method creates an Analysis objects which describes a one or more
#' models to fit to the distance data. The simulation will fit each of these
#' models to the data generated in the simulation and select the model with
#' the minimum criteria value.
#'
#' @details
#' It is possible to group strata at the analysis stage using the group.strata
#' argument. For example, for design purposes it may have been sensible to
#' divide strata into substrata. This can help make more convex shapes and
#' therefore zigzag designs more efficient or perhaps it helped to keep
#' transects angled parallel to density gradients across the study area.
#' Despite these (purely design relevant) substrata we may still wish to
#' calculate estimates of density / abundance etc. for each stratum. The
#' table below gives an example of the data.frame which can be used to do
#' this. Imagine a study region with an onshore strata and an offshore
#' strata. The onshore strata has been divided in two at the design stage
#' to keep transects perpendicular to the coast. We now want to analyse
#' this as just two strata the onshore and offshore.
#'
#' \tabular{ll}{ design.id         \tab analysis.id \cr
#'               ---------         \tab ----------- \cr
#'               onshoreN          \tab onshore     \cr
#'               onshoreS          \tab onshore     \cr
#'               offshore          \tab offshore    \cr}
#'
#' @param dfmodel list of distance sampling model formula specifying the detection function
#'  (see \code{?Distance::ds} for further details)
#' @param key key function to use; "hn" gives half-normal (default) and "hr" gives
#' hazard-rate.
#' @param truncation absolute truncation distance in simulation units matching the
#' region units.
#' @param cutpoints supply a vector of cutpoints if you wish the simulation to perform
#' binned analyses.
#' @param er.var encounter rate variance estimator to use when abundance estimates are
#'  required. Defaults to "R2" for line transects and "P3" for point transects. See
#'  \code{mrds::varn} for more information / options.
#' @param control.opts A list of control options: method - optimisation method,
#' @param group.strata Dataframe with two columns ("design.id" and "analysis.id"). The
#' former gives the strata names as defined in the design (i.e. the region object) the
#' second specifies how they should be grouped (into less strata) for the analyses. See
#' details for more information.
#' @param criteria character model selection criteria (AIC, AICc, BIC)
#' @return \code{\link{DS.Analysis-class}} object
#' @export
#' @importFrom methods new
#' @author Laura Marshall
#' @seealso \code{\link{ds}} \code{\link{make.simulation}}
#' @examples
#'
#' # Model selection considering both a half-normal and a hazard-rate model
#' # using AIC criteria and truncating 5% of the data
#' ds.analyses <- make.ds.analysis(dfmodel = ~1,
#'                                 key = c("hn", "hr"),
#'                                 truncation = 500,
#'                                 criteria = "AIC")
#'
#' # Model selection considering both a half-normal with no covariates and with size
#' # as a covariate using AIC criteria and truncating at 500
#' ds.analyses <- make.ds.analysis(dfmodel = list(~1, ~size),
#'                                 key = "hn",
#'                                 truncation = 500,
#'                                 criteria = "AIC")
#'
#' # Model selection considering both a half-normal with no covariates and with size
#' # as a covariate and a hazard rate, using AIC criteria and truncating at 500
#' ds.analyses <- make.ds.analysis(dfmodel = list(~1, ~size, ~1),
#'                                 key = c("hn", "hn", "hr"),
#'                                 truncation = 500,
#'                                 criteria = "AIC")
#'
make.ds.analysis <- function(dfmodel = list(~1),
                             key = "hn",
                             truncation = numeric(0),
                             cutpoints = numeric(0),
                             er.var = "R2",
                             control.opts = list(),
                             group.strata = data.frame(),
                             criteria = "AIC"){
  # Do some pre-creation input checking / formatting
  if(!is.double(truncation) || length(truncation) > 1){
    stop("Truncation must be supplied as a single numeric value giving the absolute truncation distance.", call. = FALSE)
  }else if("list" %in% class(truncation)){
    if(length(truncation[[1]]) > 1 || !is.double(truncation[[1]])){
      stop("Truncation must be supplied as a single numeric value giving the absolute truncation distance.", call. = FALSE)
    }
  }else{
    truncation <- list(truncation)
  }
  # make sure that the first bin starts 0 or left
  if(length(cutpoints) > 0){
    if(!is.null(truncation$left)){
      if(cutpoints[1]!=truncation$left){
        stop("The first cutpoint must be 0 or the left truncation distance!")
      }
    }else if(cutpoints[1]!=0){
      stop("The first cutpoint must be 0 or the left truncation distance!")
    }
  }
  if(!("list" %in% class(dfmodel))){
    dfmodel <- list(dfmodel)
  }
  if((length(dfmodel) > 1 && length(key) > 1) && length(dfmodel) != length(key)){
    stop("The number of df models differs from the number of key functions", call. = FALSE)
  }
  if(length(dfmodel) == 1 && length(key) > 1){
    tmp <- list()
    for(i in seq(along = key)){
      tmp[[i]] <- dfmodel[[1]]
    }
    dfmodel <- tmp
  }else if(length(dfmodel) > 1 && length(key) == 1){
    key <- rep(key, length(dfmodel))
  }
  # Create class instance
  ds.analysis <- new(Class = "DS.Analysis", dfmodel = dfmodel, key = key, truncation = truncation, cutpoints = cutpoints, er.var = er.var, control.opts = control.opts, group.strata = group.strata, criteria = criteria)
  return(ds.analysis)
}

#' @title Creates a Simulation object
#' @description This creates a simulation with all the information necessary for dsims
#' to generate a population, create transects, simulate the survey process
#' and fit detection functions and estimate density / abundance. This function can be
#' used by itself based on default values to create a simple line transect example, see
#' Examples below. To create more complex simulations it is advisable to define the
#' different parts of the simulation individually before grouping them together. See
#' the Arguments for links to the functions which make the definitions for the
#' individual simulation components. For a more in depth example please refer to the
#' 'GettingStarted' vignette.
#' @details The \code{make.simulation} function is now set up so that by
#'  default (with the exception of specifying point transects rather than
#'   line) it can run a simple simulation example. See examples.
#' @param reps number of times the simulation should be repeated
#' @param design an object of class Survey.Design created by a call to
#'  \link{make.design}
#' @param population.description an object of class Population.Description
#'  created by a call to \link{make.population.description}
#' @param detectability and object of class Detectability created by a call to
#'  \link{make.detectability}
#' @param ds.analysis an objects of class DS.Analysis created by
#'  a call to \link{make.ds.analysis}
#' @return \code{\link{Simulation-class}} object
#' @export
#' @importFrom methods new
#' @importFrom dssd make.region make.design
#' @author Laura Marshall
#' @seealso \code{\link{make.region}} \code{\link{make.density}} \code{\link{make.population.description}} \code{\link{make.detectability}} \code{\link{make.ds.analysis}} \code{\link{make.design}}
#' @examples
#' # Create a basic rectangular study area
#' region <- make.region()
#'
#' # Make a density grid (large spacing for speed)
#' density <- make.density(region = region,
#'                         x.space = 300,
#'                         y.space = 100,
#'                         constant = 1)
#' density <- add.hotspot(density, centre = c(1000, 100), sigma = 250, amplitude = 10)
#'
#' # Define the population description
#' popdsc <- make.population.description(region = region,
#'                                       density = density,
#'                                       N = 200)
#'
#' # Define the detecability
#' detect <- make.detectability(key.function = "hn",
#'                              scale.param = 25,
#'                              truncation = 50)
#'
#' # Define the design
#' design <- make.design(region = region,
#'                       transect.type = "line",
#'                       design = "systematic",
#'                       samplers = 20,
#'                       design.angle = 0,
#'                       truncation = 50)
#'
#' # Define the analyses
#' ds.analyses <- make.ds.analysis(dfmodel = ~1,
#'                                 key = "hn",
#'                                 truncation = 50,
#'                                 criteria = "AIC")
#'
#' # Put all the components together in the simulation (note no. of replicates
#' # reps = 1 is only for a single test run and should be 999 or more to be able
#' # to draw inference.)
#' simulation <- make.simulation(reps = 1,
#'                               design = design,
#'                               population.description = popdsc,
#'                               detectability = detect,
#'                               ds.analysis = ds.analyses)
#'
#' # run an example survey to check the setup
#' survey <- run.survey(simulation)
#' plot(survey, region)
#'
#' # Run the simulation
#' # Warning: if you have increased the number of replications then it can take a
#' # long time to run!
#' simulation <- run.simulation(simulation)
#' summary(simulation)
#'
#' # For a more in depth example please look at
#' vignette("GettingStarted", 'dsims')
#'
make.simulation <- function(reps = 10, design = make.design(), population.description = make.population.description(), detectability = make.detectability(), ds.analysis = make.ds.analysis()){

  # Make the results arrays and store in a list
  results <- create.results.arrays(reps,
                                   design@region,
                                   ds.analysis,
                                   population.description)
  # Create a simulation object
  simulation <- new(Class = "Simulation",
                    reps = reps,
                    design = design,
                    population.description = population.description,
                    detectability = detectability,
                    ds.analysis = ds.analysis,
                    results = results)
  # Check the simulation object
  simulation <- check.simulation(simulation)
  # If it has returned a character this is an error
  if(class(simulation) == "character"){
    stop(simulation, call. = FALSE)
  }

  return(simulation)
}
