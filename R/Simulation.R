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
#' @slot ddf.param.ests Object of class \code{"array"}; stores the
#'  parameters associated with the detection function.
#' @slot results A \code{"list"} of \code{"arrays"}; stores
#'  the estimated of abundance and density as well as other measures
#'  of interest.
#' @slot warnings A \code{"list"} to store warnings and error messages encountered
#'  during runtime.
#' @section Methods:
#' \describe{
#'  \item{\code{add.hotspot}}{\code{signature=(object = "Simulation")}: adds
#'  a hotspot based on a gaussian decay to the density surface.}
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
                                      single.transect.set = "logical",
                                      design = "Survey.Design",
                                      population.description = "Population.Description",
                                      detectability = "Detectability",
                                      ds.analyses = "DS.Analysis",
                                      ddf.param.ests = "array",
                                      results = "list",
                                      warnings = "list"))

setMethod(
  f="initialize",
  signature="Simulation",
  definition=function(.Object, reps = 10, single.transect.set = FALSE, double.observer = FALSE, region = make.region(), design = make.design(), population.description = make.population.description(), detectability = make.detectability(), ddf.analyses = make.ddf.analysis.list(), results = list()){
    #Set slots
    .Object@reps            <- reps
    .Object@single.transect.set <- single.transect.set
    .Object@design          <- design
    .Object@population.description <- population.description
    .Object@detectability   <- detectability
    .Object@ds.analysis    <- ds.analysis
    .Object@results         <- results
    .Object@warnings        <- list()
    #Check object is valid
    validObject(.Object)
    # return object
    return(.Object)
  }
)

setValidity("Simulation",
            function(object){
              #if(object@double.observer){
              #  warning("Double observer simulations not supported at present", call. = TRUE, immediate. = TRUE)
              #  return(FALSE)
              #}
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
                                      region.obj = object@design@region)
    return(population)
  }
)

#' @name generate.transects
#' @description Calls the generate.transects method inside dssd on the
#' design object inside the simulation. It then returns a single set
#' of transects created based on the design.
#' @seealso \code{?dssd::generate.transects}
#' @rdname generate.transects-methods
#' @export
setMethod(
  f="generate.transects",
  signature="Simulation",
  definition=function(object, region = NULL){
    transect <- generate.transects(object = object@design,
                                   region = object@design@region)
    return(transect)
  }
)

#' @rdname run.survey-methods
#' @export
setMethod(
  f="run.survey",
  signature="Simulation",
  definition=function(object){
    # Create the population and transects for the survey
    population <- generate.population(object)
    transects <- generate.population(object)
    if(class(transects) == "Line.Transect"){
      survey <- new(Class = "Survey.LT",
                    population = pop,
                    transect = transects,
                    perp.truncation = object@design@truncation)
    }else if(class(transects) == "Point.Transect"){
      survey <- new(Class = "Survey.PT",
                    population = pop,
                    transect = transects,
                    rad.truncation = object@design@truncation)
    }
    # Simulate the survey
    survey <- run.survey(survey, region = object@design@region)
    return(survey)
  }
)


