#' @import Distance
#' @importFrom dssd generate.transects
NULL

#' S4 generic method to generate an instance of a population
#'
#' Uses the population description and detectability details to generate an
#' instance of the population. Note that if the first argument supplied is
#' of class Population.Description rather than class Simulation then
#' detectability and region must also be supplied.
#'
#' @param object an object of class Simulation or Population.Description
#' @param ... when this is called on an object of class Population.Description
#' the additional arguments detectability and region.obj should also be supplied
#' @return an object of class Population
#' @export
#' @rdname generate.population-methods
setGeneric("generate.population", function(object, ...){standardGeneric ("generate.population")})


#' S4 generic method to simulate a survey
#'
#' Simulates the process by which individuals or clusters are detected. If
#' a simulation is passed in then it will generate a population, set of
#' transects and simulate the detection process. If a survey is passed in
#' it will simply simulate the detection process. See
#' \code{\link{make.simulation}} for example usage.
#'
#' @param object an object of class Simulation
#' @param ... allows extra arguments
#' @return an object of class Survey
#' @export
#' @rdname run.survey-methods
#' @seealso \code{\link{make.simulation}}
setGeneric(name = "run.survey", def = function(object, ...){standardGeneric ("run.survey")})

#' S4 generic method to extract distance data
#'
#' Extracts distance data from a Survey.Results object
#'
#' @param object an object of class LT.Survey.Results
#' @return a list containing a data.frame of distance data and a vector of all
#' the possible detection distances. The latter can be used to calculate the
#' number of animals / clusters in the covered area for truncation distances
#' equal to or less than those used in the design.
#' @export
#' @rdname get.distance.data-methods
#' @seealso \code{\link{run.survey}}
setGeneric(name = "get.distance.data", def = function(object){standardGeneric ("get.distance.data")})

#' S4 generic method to add a hotspot to the density grid
#'
#' Uses a Gaussian decay around a central location to add a hotspot to the
#' density grid.
#'
#' @param object an object of class Density or Simulation
#' @param centre an x,y-coordinate giving the centre of the hotspot
#' @param sigma a value giving the scale parameter for a gaussian decay
#' @param amplitude the height of the hotspot at its centre
#' @return the updated Density or Simulation object
#' @export
#' @rdname add.hotspot-methods
#' @seealso \code{\link{make.density}}
setGeneric(name = "add.hotspot", def = function(object, centre, sigma, amplitude){standardGeneric ("add.hotspot")})


#' S4 generic method to run analyses
#'
#' This method carries out an analysis of distance sampling data. This method
#' is provided to allow the user to perform diagnostics of the analyses used
#' in the simulation. The data argument can be obtained by a call to
#' \code{simulate.survey(object, dht.table = TRUE)}. Note if the first object
#' supplied is of class DS.Analysis then the second argument must be of class
#' DDf.Data. The data argument may be of either class for an object argument
#' of class Simulation.
#'
#' @param analysis an object of class DS.Analysis
#' @param data.obj an object of class Survey or a dataframe
#' @param ... optional arguments including the following:
#' @return a list containing an S3 ddf object and optionally an S3 dht object relating to the model with the minimum criteria.
#' @export
#' @rdname analyse.data-methods
setGeneric(name = "analyse.data", def = function(analysis, data.obj, ...){standardGeneric ("analyse.data")})


if (!isGeneric("plot")){
  setGeneric(name = "plot", def = function(x, y, ...){standardGeneric("plot")})
}

if (!isGeneric("summary")){
  setGeneric(name = "summary", def = function(object, ...){standardGeneric("summary")})
}

if (!isGeneric("show")){
  setGeneric(name = "show", def = function(object){standardGeneric("show")})
}


