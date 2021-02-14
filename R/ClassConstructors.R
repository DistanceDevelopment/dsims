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
#' with x and y as explanatory variables. Additionally the user has the option to
#' create an equally spaced grid themselves describing the density of objects at
#' eack grid point. There should be one data.frame with x, y and density values per
#' strata and these are grouped together in a list. Care must be taken to then provide
#' make.density with the correct x and y spacing values. If you plan on trying multiple
#' animal distributions by adding high and low areas to a constant surface if is
#' recommended to make a copy of the initial flat density grid object as the first
#' step in grid generation is computationally intensive and can take a little while
#' to complete, especially if you have a fine density grid.
#' @param region the Region object in which the density grid will be created
#' @param x.space the intervals in the grid in the x direction
#' @param y.space the intervals in the grid in the y direction
#' @param buffer the width of the buffer region for generating the density grid. If not supplied DSsim will use the maximum value provided for the x.space or y.space.
#' @param constant a value describing a constant density across the surface. If not supplied a default value of 1 is used for all strata.
#' @param density.gam \code{gam} object created using \code{mgcv} with only x and y as explanatory covariates.
#' @param density.surface Object of class \code{list}; list of
#'  data.frames with the columns x, y and density. There must be one
#'  data.frame for each strata.
#' @return object of class Density
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
#' plot(density)
#'
make.density <- function(region = make.region(), x.space = 20, y.space = NULL, buffer = numeric(0), constant = numeric(0), density.gam = NULL, density.surface = list(),){
  # Check if the user has supplied a y.space value
  if(is.null(y.space)){
    # If not set it equal to x.space
    y.space <- x.space
  }
  # Check how many grid points there will likely be
  est.grid.points <- (sum(region@area)/(x.space)^2)*(x.space/y.space)
  if(est.grid.points < 25){
    stop("Your grid spacing will result in less than 25 grid cells. Please reduce your x.space (and optionally your y.space) argument.", call. = FALSE)
  }else if(est.grid.points > 25000){
    stop("Your grid spacing will result in more than 25,000 grid cells. Please increase your x.space (and optionally your y.space) argument.", call. = FALSE)
  }else if(est.grid.points > 5000){
    warning("Your grid spacing arguments x.space (and y.space) will result in more than 5000 grid cells being generated. This could take a little time!", call. = FALSE, immediate. = TRUE)
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
  density <- new(Class = "Density", region = region, strata.name = region@strata.name, density.surface = density.surface, x.space = x.space, y.space = y.space, constant = constant, density.gam = density.gam, buffer = buffer)
  return(density)
}

#' @title Creates a Population.Description object
#'
#' @description
#' Creates an object which describes a population. The values in this object
#' will be used to create instances of the population.
#'
#' @details
#' The \code{covariates} argument should specify a list with one named
#' element per covariate. The population covariate values can either be described
#' by dataframes representing discrete covariates or by distributions.
#' If specifying the covariate values via a distribution
#' this should be done in the form of a list, the list should first contain the
#' distribution and then specify
#'
#' The first element should be one of
#' the following: 'normal', 'poisson', 'ztruncpois' or 'lognormal'. The 'ztruncpois'
#' distribution refers to a zero truncated Poisson distribution. The corresponding
#' parameters that you must supply are detailed below. These should be added to a named
#' list (each element named with the parameter name) containing the parameter values.
#' See examples for implementation.
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
#' @param covariates Named list with one named entry per individual level covariate. Cluster sizes can be defined here, it must be named 'size'. Each list entry should be another list with either one element or one element per strata allowing different population structures per strata. Each element of these lists should either be a data.frame containing 2 columns, the first the level (level) and the second the probability (prob). The cluster size entry in the list must be named 'size'. Alternatively the list element may be another list specifying the distribution in the first element and a named list in the second element with the distribution parameter.
#' @param N the number of individuals / clusters in a population (1000 by default)
#' @param fixed.N a logical value. If TRUE the population is generated from the value of N
#' otherwise it is generated from the density description.
#' @return object of class Population.Description
#' @export
#' @importFrom methods new
#' @importFrom dssd make.region
#' @author Laura Marshall
#' @seealso \code{\link{make.region}}, \code{\link{make.density}}, \code{\link{make.detectability}}
#' @examples
#' # An example population can be created from the default values:
#' # - the default region
#' # - a constant density surface
#' # - and a population size of 1000
#' pop.desc <- make.population.description()
#'
#' # To view an instance of this population
#' pop <- generate.population(pop.desc, make.detectability(), make.region())
#' plot(make.region())
#' plot(pop)
#'
#' # An example population with covariates which vary by strata
#' # Make a multi strata region
#' poly1 <- data.frame(x = c(0,0,100,100,0), y = c(0,100,100,0,0))
#' poly2 <- data.frame(x = c(200,200,300,300,200), y = c(10,110,110,10,10))
#' coords <- list(list(poly1), list(poly2))
#' region <- make.region(coords = coords)
#' density <- make.density(region)
#'
#' # Cluzter size is a zero truncated poisson with mean = 5 in strata 1 and a poisson with
#' # lambda = 30 in strata 2.
#' covariate.list <- list()
#' covariate.list$size <- list(list("ztruncpois", list(mean = 5)),
#'                             list("poisson", list(lambda = 30)))
#'
#' # Animal height is generated from a lognormal distribution for both strata
#' covariate.list$height <- list(list("lognormal", list(meanlog = log(2), sdlog = log(1.25))))
#'
#' # Animal sex is discrete/categorical, there are more females than males in strata 1 and equal
#' # numbers in strata 2
#' covariate.list$sex <- list(data.frame(level = c("male", "female"), prob = c(0.45,0.55)),
#'                            data.frame(level = c("male", "female"), prob = c(0.5,0.5)))
#'
#' # Create covariate description
#' pop.desc <- make.population.description(region.obj = region,
#'                                         density.obj = density,
#'                                         covariates = covariate.list,
#'                                         N = c(10,10))
#'
#' # To view the covariate values
#' pop <- generate.population(pop.desc, detect = make.detectability(), region)
#' pop@population
#' # Note that the covariate values have not affected the detectability (the scale parameter) to
#' # do this we need to set the cov.param argument in make.detectability. See ?make.detectability
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
          if(sort(strat.names) != sort(strat.names.check)){
            stop(paste("The strata names in the covariate dataframe for ", cov.names[cov], " do not match the strata names in the region object.", sep = ""), call. = FALSE)
          }
          # Separte this table out into separate list elements
          for(j in seq(along = strat.names)){
            cov.dataframe <- covariates[[cov]][[i]]
            strat.list[[j]] <- cov.dataframe[cov.dataframe$strata == strat.names[j],]
          }
        }
      }else if(class(covariates[[cov]][[i]]) == "list"){
        # Find what parameters should be supplied given the distribution
        params <- switch(covariates[[cov]][[i]]$distribution,
                         normal = c("mean", "sd"),
                         poisson = "lambda",
                         ztruncpois = "mean",
                         lognormal = c("meanlog", "sdlog"))
        if(!params %in% names(covariates[[cov]][[i]])){
          stop(paste("You have not supplied all the required parameters (", paste(params, collapse = ", "),") for the following covariate distribution: ", covariates[[cov]][[i]]$distribution, sep = ""))
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

#' @title Creates a Detectablility object
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
#' @return object of class Detectablility
#' @export
#' @importFrom methods new
#' @author Laura Marshall
#' @examples
#' # The default values create a detectability object with a half normal
#' # detection function with scale parameter 25 and truncation distance 50.
#' detect <- make.detectability()
#' detect
#'
#' # To include covariate parameters which affect detecability,
#' # first you need to make sure the population has covariates defined
#' # see examples in ?make.population.description
#' # Multi-strata covariate example
#' # Make a multi strata region
#' poly1 <- data.frame(x = c(0,0,100,100,0), y = c(0,100,100,0,0))
#' poly2 <- data.frame(x = c(200,200,300,300,200), y = c(10,110,110,10,10))
#' coords <- list(list(poly1), list(poly2))
#' region <- make.region(coords = coords)
#' density <- make.density(region)
#' # Create the population description
#' covariate.list <- list()
#' covariate.list$size <- list(list("ztruncpois", list(mean = 3)),
#'                             list("ztruncpois", list(mean = 5)))
#' covariate.list$height <- list(list("lognormal", list(meanlog = log(2), sdlog = log(1.25))))
#' covariate.list$sex <- list(data.frame(level = c("male", "female"), prob = c(0.45,0.55)),
#'                            data.frame(level = c("male", "female"), prob = c(0.5,0.5)))
#' pop.desc <- make.population.description(region.obj = region,
#'                                         density.obj = density,
#'                                         covariates = covariate.list,
#'                                         N = c(10,10))
#'
#' # In this example height and sex have a global effect where as the effects of size on
#' # detectability vary by strata.
#' cov.params <- list(size = c(log(1.05), log(1.1)),
#'                    height = log(1.2),
#'                    sex = data.frame(level = c("male", "female"),
#'                                     param = c(log(1), log(0.6))))
#'
#' detect <- make.detectability(key.function = "hn", scale.param = 20,
#'                              truncation = 50, cov.param = cov.params)
#'
#' plot(detect, pop.desc)
#'
#' # If we want the effects of sex to be strata specific we can define detectability as follows:
#' cov.params <- list(size = c(0.05, 0.1),
#'                    height = 0.2,
#'                    sex = data.frame(level = c("male", "female","male", "female"),
#'                                     strata = c("A", "A", "B", "B"),
#'                                     param = c(0,-0.5, 0.1, -0.25)))
#'
#' detect <- make.detectability(key.function = "hn", scale.param = c(20, 25),
#'                              truncation = 60, cov.param = cov.params)
#' plot(detect, pop.desc)
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
#' @param dfmodel list of distance sampling model formula specifying the detection function
#'  (see \code{?Distance::ds} for further details)
#' @param key key function to use; "hn" gives half-normal (default) and "hr" gives
#' hazard-rate.
#' @param adjustment a way of providing information about the adjustment term options. A
#' list of options for adjustment parameters. In the case of multiple models this should
#' be a list of option lists, one for each model. Note adjustment terms can only be
#' included when there are no covariates in the model. The adjustment options include:
#' adjustment - "cos" (recommended), "herm" or "poly", order - the order of the adjustment
#' terms and scale - either "width" or "scale". See details for more information.
#' @param truncation distance can be supplied as (numeric, e.g. 5) or percentage (as a
#' string, e.g. "15\%"). By default for exact distances the maximum observed
#' distance is used as the right truncation. Note that any value supplied as a string
#' will be interpreted as a \% even without the \% symbol.
#' @param cutpoints if the data are binned, this vector gives the cutpoints of the bins.
#'  Ensure that the first element is 0 (or the left truncation distance) and the last
#'  is the distance to the end of the furthest bin. (Default NULL, no binning.) Note
#'  that if data has columns distbegin and distend then these will be used as bins if cutpoints is not specified. If both are specified, cutpoints has precedence.
#' @param er.var encounter rate variance estimator to use when abundance estimates are
#'  required. Defaults to "R2" for line transects and "P3" for point transects. See
#'  \code{mrds::varn} for more information / options.
#' @param control.opts A list of control options: method - optimisation method,
#'  initial.values - a list of names starting values, see mrds
#' @param group.strata Dataframe with two columns ("design.id" and "analysis.id"). The
#' former gives the strata names as defined in the design (i.e. the region object) the
#' second specifies how they should be grouped (into less strata) for the analyses
#' @param criteria character model selection criteria (AIC, AICc, BIC)
#' @return an object of class DS.Analysis
#' @export
#' @importFrom methods new
#' @author Laura Marshall
#' @seealso \code{ds} in \code{library(Distance)}
#' @examples
#'
#' # To incorporate model selection between a 'hn' and 'hr' model:
#' ds.analyses <- make.ds.analysis(dfmodel = ~1, key = "hn")
#'
make.ds.analysis <- function(dfmodel = list(~1),
                             key = "hn",
                             adjustment = list(),
                             truncation = numeric(0),
                             cutpoints = numeric(0),
                             er.var = "R2",
                             control.opts = list(),
                             group.strata = data.frame(),
                             criteria = "AIC"){
  # Do some pre-creation input checking / formatting
  # if(length(cutpoints) > 0 && length(truncation) > 0){
  #   warning("Cutpoints have been supplied so the truncation value(s) will be ignored. The largest cutpoint will be used as the right truncation value", call. = FALSE, immediate. = TRUE)
  #   # Doing this means left trunction is not currently possible with binned data
  #   truncation <- numeric(0)
  # }
  if("list" %in% class(truncation)){
    if(!all(c("left","right") %in% names(truncation))){
      stop("Truncation must be supplied as a single number/string or a list with elements \"left\" and \"right\".", call. = FALSE)
    }
  }else{
    if(length(truncation) > 1){
      stop("Truncation must be supplied as a single number/string or a list with elements \"left\" and \"right\".", call. = FALSE)
    }
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
  # Create class instance
  ds.analysis <- new(Class = "DS.Analysis", dfmodel = dfmodel, key = key, adjustment = adjustment, truncation = truncation, cutpoints = cutpoints, er.var = er.var, control.opts = control.opts, group.strata = group.strata, criteria = criteria)
  return(ds.analysis)
}

#' @title Creates a Simulation object
#' @description This creates a simulation with all the information necessary for DSsim
#' to generate a population, create or read in transects, simulate the survey process
#' and fit detection functions and estimate density / abundance. This function can be
#' used by itself based on default values to create a simple line transect example, see
#' Examples below. To create more complex simulations it is advisable to define the
#' different parts of the simulation individually before grouping them together. See
#' the Arguments for links to the functions which make the definitions for the
#' individual simulation components.
#' @details The \code{make.simulation} function is now set up so that by
#'  default (with the exception of specifying point transects rather than
#'   line) it can run a simple simulation example. See examples.
#' @param reps number of times the simulation should be repeated
#' @param design an object of class Survey.Design created by a call to
#'  \link{make.design}
#' @param population.description an object of class Population.Description
#'  created by a call to \link{make.population.description}
#' @param detectability and object of class Detectabolity created by a call to
#'  \link{make.detectability}
#' @param ds.analysis a list of objects of class DS.Analysis created by
#'  a call to\link{make.ds.analysis}
#' @return object of class Simulation
#' @export
#' @importFrom methods new
#' @importFrom dssd make.region make.design
#' @author Laura Marshall
#' @examples
#' \dontrun{
#' # A basic line transect simulation example
#' sim <- make.simulation()
#' check.sim.setup(sim)
#' sim <- run(sim)
#' summary(sim)
#'
#' # A basic point transect simulation example
#' sim <- make.simulation(design.obj = make.design("point"))
#' check.sim.setup(sim)
#' sim <- run(sim)
#' summary(sim)
#' # Note % bias levels will vary due to low number of repetitions
#' # set by default in these examples
#'
#' # To increase the number of repetitions
#' sim <- make.simulation(reps = 100)
#' sim <- run(sim)
#' summary(sim)
#' }
#'
#' coords <- gaps <- list()
#' coords[[1]] <- list(data.frame(x = c(0,1000,1000,0,0), y = c(0,0,
#'  1000,1000,0)))
#' gaps[[1]] <- list(data.frame(x = c(400,600,500,350,400), y = c(100,
#'  250,600,120,100)))
#'
#' region <- make.region(region.name = "study.area", units = "m",
#'  coords = coords, gaps = gaps)
#' plot(region)
#'
#' \dontrun{
#' data(transects.shp)
#' #Edit the pathway below to point to an empty folder where the
#' #transect shapefile will be saved
#' shapefile.pathway <- "C:/..."
#' write.shapefile(transects.shp, paste(shapefile.pathway,"/transects_1",
#'  sep = ""))
#'
#' parallel.design <- make.design(transect.type = "Line",
#'  design.details = c("Parallel","Systematic"), region = region,
#'  design.axis = 0, spacing = 100, plus.sampling =FALSE,
#'  path = shapefile.pathway)
#'
#' pop.density <- make.density(region.obj = region, x.space = 10,
#'  y.space = 10, constant = 0.5)
#' pop.density <- add.hotspot(pop.density, centre = c(50, 200),
#'  sigma = 100, amplitude = 0.1)
#' pop.density <- add.hotspot(pop.density, centre = c(500, 700),
#'  sigma = 900, amplitude = 0.05)
#' pop.density <- add.hotspot(pop.density, centre = c(300, 100),
#'  sigma = 100, amplitude = -0.15)
#'
#' plot(pop.density)
#' plot(region, add = TRUE)
#'
#' pop.description <- make.population.description(N = 1000,
#'  density.obj = pop.density, region = region, fixed.N = TRUE)
#'
#' detect <- make.detectability(key.function = "hn", scale.param = 15,
#'  truncation = 30)
#'
#' ddf.analyses <- make.ddf.analysis.list(dfmodel = list(~cds(key = "hn",
#'  formula = ~1),~cds(key = "hr", formula = ~1)), method = "ds",
#'  criteria = "AIC")
#'
#' my.simulation <- make.simulation(reps = 10, single.transect.set = TRUE,
#'  region.obj = region, design.obj = parallel.design,
#'  population.description.obj = pop.description,
#'  detectability.obj = detect, ddf.analyses.list = ddf.analyses)
#'
#' survey <- run.survey(my.simulation)
#'
#' plot(survey.results)
#'
#' my.simulation <- run(my.simulation)
#'
#' summary(my.simulation)
#' }
#'
make.simulation <- function(reps = 10, design = make.design(), population.description = make.population.description(), detectability = make.detectability(), ds.analysis = make.ds.analysis()){

  # Make the results arrays and store in a list
  results <- create.results.arrays(reps,
                                   design@region,
                                   ds.analysis,
                                   population.description)
  #create a simulation object
  simulation <- new(Class = "Simulation",
                    reps = reps,
                    design = design,
                    population.description = population.description,
                    detectability = detectability,
                    ds.analysis = ds.analysis,
                    results = results)
  return(simulation)
}
