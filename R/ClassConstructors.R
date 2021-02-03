#' @include Simulation.R
#' @include Population.Description.R
#' @include Detectability.R
#' @include Density.R
#' @include DS.Analysis.R

#' @title Creates a Density object
#' @description
#' The user has the option to create a grid describing the density of the
#' objects and pass this in giving the x and y spacings used in the creation
#' of this grid. Alternatively the user can specify a constant density and x,
#' y spacings and this grid will be generated automatically. The user may
#' also supply a \code{mgcv gam} object and x, y spacings and the density grid will
#' be created from these.
#'
#' @param region.obj the Region object in which the density grid will be created
#' @param density.surface Object of class \code{list}; list of
#'  data.frames with the columns x, y and density. There must be one
#'  data.frame for each strata.
#' @param x.space the intervals in the grid in the x direction
#' @param y.space the intervals in the grid in the y direction
#' @param buffer the width of the buffer region for generating the density grid. If not supplied DSsim will use the maximum value provided for the x.space or y.space.
#' @param constant a value describing a constant density across the surface. If not supplied a default value of 1 is used for all strata.
#' @param density.gam \code{gam} object created using \code{mgcv} with only x and y as explanatory covariates.
#' @param dsm not currently implemented
#' @param formula not currently implemented
#' @return object of class Density
#' @export
#' @author Laura Marshall
#' @seealso \code{\link{make.region}}
#' @examples
#' # A simple density surface with a constant value of 1 can be created within a rectangular
#' # region using
#' # the default values:
#' density <- make.density()
#' plot(density)
#' plot(make.region(), add = TRUE)
#'
#' # The example below shows hot to add high and low point to the density surface
#' \dontrun{
#' pop.density <- make.density(region.obj = region, x.space = 10,
#'  y.space = 10, constant = 0.5)
#'
#' pop.density <- add.hotspot(pop.density, centre = c(50, 200),
#'  sigma = 100, amplitude = 0.1)
#' pop.density <- add.hotspot(pop.density, centre = c(500, 700),
#'  sigma = 900, amplitude = 0.05)
#' pop.density <- add.hotspot(pop.density, centre = c(300, 100),
#'  sigma = 100, amplitude = -0.15)
#'
#' #New plot features
#' plot(pop.density)
#' plot(region, add = TRUE)
#'
#' #Block style plotting
#' plot(pop.density, contours = FALSE, style = "blocks")
#' plot(region, add = TRUE)
#'
#' }
make.density <- function(region.obj = make.region(), density.surface = list(), x.space = 5, y.space = NULL, buffer = numeric(0), constant = numeric(0), density.gam = NULL, dsm = NULL, formula = NULL){
  # Find the number of strata
  no.strata <- length(region.obj@strata.name)
  # Check the user has supplied the correct number of consants
  if(length(constant) > 0){
    if(no.strata > 0 & length(constant) != length(region.obj@strata.name)){
      if(length(constant) == 1){
        constant <- rep(constant, length(region.obj@strata.name))
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
  # Check if the user has supplied a y.space value
  if(is.null(y.space)){
    # If not set it equal to x.space
    y.space <- x.space
  }
  # Make density object
  density <- new(Class = "Density", region = region.obj, strata.name = region.obj@strata.name, density.surface = density.surface, x.space = x.space, y.space = y.space, constant = constant, density.gam = density.gam, buffer = buffer)
  return(density)
}

#' @title Creates a Population.Description object
#' @description
#' Creates an object which describes a population. The values in this object
#' will be used to create instances of the population
#'
#' @details #' The \code{covariates} argument should specify a list with one named
#' element per covariate. If specifying the covariate values via a distribution
#' this should be done in the form of a list. The first element should be one of
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
#' @param region.obj the Region object in which this population exists (see \link{make.region}).
#' @param density.obj the Density object describing the distribution of the individuals / clusters (see \link{make.density}).
#' @param covariates Named list with one named entry per individual level covariate. Cluster sizes can be defined here. Each list entry should be another list with either one element or one element per strata allowing different population structures per strata. Each element of these lists should either be a data.frame containing 2 columns, the first the level (level) and the second the probability (prob). The cluster size entry in the list must be named 'size'. Alternatively the list element may be another list specifying the distribution in the first element and a named list in the second element with the distribution parameter.
#' @param N the number of individuals / clusters in a population (1000 by default)
#' @param fixed.N a logical value. If TRUE the population is generated from the value of N
#' otherwise it is generated from the density description.
#' @return object of class Population.Description
#' @export
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
make.population.description <- make.pop.description <- function(region.obj = make.region(), density.obj = make.density(), covariates = list(), N = numeric(0), fixed.N = TRUE){
  pop.description <- new(Class = "Population.Description", N = N, density = density.obj, region.obj = region.obj, covariates = covariates, gen.by.N = fixed.N)
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
#'
#' @param dsmodel list of distance sampling model formula specifying the detection function
#'  (see \code{?ds} for further details)
#' @param key key function to use; "hn" gives half-normal (default) and "hr" gives
#' hazard-rate.
#' @param adjustment a way of providing information about the adjustment term options. A
#' list of options for adjustment parameters. In the case of multiple models this should
#' be a list of option lists, one for each model. Note adjustment terms can only be
#' included when there are no covariates in the model. The adjustment options include:
#' adjustment - "cos" (recommended), "herm" or "poly", order - the order of the adjustment
#' terms and scale - either "width" or "scale". See details for more information.
#' @param truncation distance can be supplied as (numeric, e.g. 5) or percentage (as a
#' string, e.g. "15%"). By default for exact distances the maximum observed
#' distance is used as the right truncation. Note that any value supplied as a string
#' will be interpreted as a % even without the % symbol.
#' @param cutpoints if the data are binned, this vector gives the cutpoints of the bins.
#'  Ensure that the first element is 0 (or the left truncation distance) and the last
#'  is the distance to the end of the furthest bin. (Default NULL, no binning.) Note
#'  that if data has columns distbegin and distend then these will be used as bins if cutpoints is not specified. If both are specified, cutpoints has precedence.
#' @param er.var encounter rate variance estimator to use when abundance estimates are
#'  required. Defaults to "R2" for line transects and "P3" for point transects. See dht2
#'  for more information and if more complex options are required.
#' @param control.opts A list of control options: method - optimisation method,
#'  initial.values - a list of names starting values, see mrds
#' @param group.strata Dataframe with two columns ("design.id" and "analysis.id"). The
#' former gives the strata names as defined in the design (i.e. the region object) the
#' second specifies how they should be grouped (into less strata) for the analyses
#' @param criteria character model selection criteria (AIC, AICc, BIC)
#' @return an object of class DS.Analysis
#' @export
#' @author Laura Marshall
#' @seealso \code{ds} in \code{library(Distance)}
#' @examples
#' # A simple half-normal "ds" model can be created using the default values
#' ddf.analyses <- make.ddf.analysis.list()
#'
#' # To incorporate model selection between a 'hn' and 'hr' model:
#' ddf.analyses <- make.ddf.analysis.list(dsmodel = list(~cds(key = "hn",
#'  formula = ~1),~cds(key = "hr", formula = ~1)), method = "ds",
#'  criteria = "AIC")
#'
make.ds.analysis <- function(dsmodel = list(~1),
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


  if(!("list" %in% class(dsmodel))){
    dsmodel <- list(dsmodel)
  }
  # Create class instance
  ds.analysis <- new(Class = "DS.Analysis", dsmodel = dsmodel, key = key, adjustment = adjustment, truncation = truncation, cutpoints = cutpoints, er.var = er.var, control.opts = control.opts, group.strata = group.strata, criteria = criteria)
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
#' individual simulation components. Example simulations can also be found at
#' <https://github.com/DistanceDevelopment/DSsim/wiki>.
#' @details The \code{make.simulation} function is now set up so that by
#'  default (with the exception of specifying point transects rather than
#'   line) it can run a simple simulation example. See examples.
#' @param reps number of times the simulation should be repeated
#' @param single.transect.set logical specifying whether the transects should
#'   be kept the same throughout the simulation.
#' @param double.observer not currently implemented.
#' @param region.obj an object of class Region created by a call to
#'  \link{make.region}
#' @param design.obj an object of class Survey.Design created by a call to
#'  \link{make.design}
#' @param population.description.obj an object of class Population.Description
#'  created by a call to \link{make.population.description}
#' @param detectability.obj and object of class Detectabolity created by a call to
#'  \link{make.detectability}
#' @param ddf.analyses.list a list of objects of class DDF.Analysis created by
#'  a call to\link{make.ddf.analysis.list}
#' @return object of class Simulation
#' @export
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
#' ddf.analyses <- make.ddf.analysis.list(dsmodel = list(~cds(key = "hn",
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
  # Check to see if the analysis truncation distance is larger than the
  # detectability trunation distance
  if(ds.analysis@truncation > detectability@truncation){
    warning("The truncation distance for analysis is larger than the truncation distance for data generation, this will likely cause biased results.", immediate. = TRUE, call. = FALSE)
  }
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
