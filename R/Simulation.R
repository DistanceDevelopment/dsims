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
#' @slot double.observer Object of class \code{"logical"}; whether
#'  a double observer protocol is being used. Not currently implemented.
#' @slot region Object of class \code{"Region"}; the survey region.
#' @slot design Object of class \code{"Survey.Design"}; the
#'  survey design.
#' @slot population.description Object of class \code{"Population.Description"};
#' the population.description.
#' @slot detectability Object of class \code{"Detectability"}; a
#'  description of the detectability of the population.
#' @slot ddf.analyses Object of class \code{"list"}; a list of
#'  objects of class DDF.Analysis. These are fitted and the one with the
#'  minimum criteria is selected and used in predicting N and D.
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
#'  \item{\code{create.survey.results}}{\code{signature = (object =
#'  "Simulation")}: carries out the simulation process as far as generating
#'  the distance data and returns an object containing the population,
#'  transects and data.}
#'  \item{\code{run.analysis}}{\code{signature = c(object =
#'  "Simulation", data = "LT.Survey.Results")}: returns the ddf analysis
#'  results from the models in the simulation fitted to the data in the
#'  LT.Survey.Results object.}
#'  \item{\code{run.analysis}}{\code{signature = c(object =
#'  "Simulation", data = "DDF.Data")}: returns the ddf analysis
#'  results from the models in the simulation fitted to the data in the
#'  DDF.Data object.}
#'  \item{\code{run}}{\code{signature = (object = "Simulation")}: runs
#'  the whole simulation for the specified number of repetitions.}
#' }
#' @keywords classes
#' @rdname Simulation-class
#' @importClassesFrom dssd Region
#' @seealso \code{\link{make.simulation}}
setClass("Simulation", representation(reps = "numeric",
                                      single.transect.set = "logical",
                                      double.observer = "logical",
                                      region = "Region",
                                      design = "Survey.Design",
                                      population.description = "Population.Description",
                                      detectability = "Detectability",
                                      ddf.analyses = "list",
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
    .Object@double.observer <- double.observer
    .Object@region          <- region
    .Object@design          <- design
    .Object@population.description <- population.description
    .Object@detectability   <- detectability
    .Object@ddf.analyses    <- ddf.analyses
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
              if(object@double.observer){
                warning("Double observer simulations not supported at present", call. = TRUE, immediate. = TRUE)
                return(FALSE)
              }
              design <- object@design
              if(class(design) == "PT.Nested.Design"){
                if(object@detectability@truncation > object@ddf.analyses[[1]]@truncation){
                  warning("Please be aware that your truncation distance for analysis is less than that used to generate the data (defined in detectability). This will introduce bias into your estimates.", call. = FALSE, immediate. = TRUE)
                }
              }
              transects.from.file <- ifelse(length(design@path) == 1, TRUE, FALSE)
              if(transects.from.file & !object@single.transect.set){
                no.files <- length(design@filenames)
                if(object@reps > no.files){
                  message("You have specified a higher number of repetitions than you have provided transect shapefiles at: ", design@path)
                  return(FALSE)
                }
              }else if(!transects.from.file){
                if(!(class(object@design) %in% c("PT.Nested.Design", "PT.Systematic.Design", "LT.Systematic.Design"))){
                  message("The generation of transects is only implemented in R for nested and systematic point transect designs as well as systematic parallel line transect designs.")
                  return(FALSE)
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
    population <- generate.population(object = object@population.description, detectability = object@detectability, region.obj = object@region)
    return(population)
  }
)

#' @rdname generate.transects-methods
#' @export
setMethod(
  f="generate.transects",
  signature="Simulation",
  definition=function(object, region = NULL){
    region <- object@region
    transect <- generate.transects(object@design, region = region)
    return(transect)
  }
)
