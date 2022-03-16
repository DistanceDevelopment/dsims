#' @include generic.functions.R
#' @include Density.R

#' @title Class "Population.Description"
#'
#' @description Class \code{"Population.Description"} is an S4 class containing a
#' description of the population. It provides methods to generate an
#' example population.
#'
#' @name Population.Description-class
#' @title S4 Class "Population.Description"
#' @slot N Object of class \code{"numeric"}; number of individuals
#' in the population (optional).
#' @slot density Object of class \code{"Density"}; describes the
#' population density
#' @slot region.name Object of class \code{"character"}; name of
#' the region in which the population exists.
#' @slot strata.names Character vector giving the strata names for the study region.
#' @slot covariates Named list with one named entry per individual level covariate.
#' Cluster sizes can be defined here. Each list entry will either be a data.frame
#' containing 2 columns, the first the level (level) and the second the probability
#' @slot size logical value indicating whether the population occurs in
#' clusters.
#' (prob). The cluster size entry in the list must be named 'size'.
#' @slot gen.by.N Object of class \code{"logical"}; If \code{TRUE}
#' N is fixed otherwise it is generated from a Poisson distribution.
#' @section Methods:
#' \describe{
#'  \item{\code{get.N}}{\code{signature=(object = "Population.Description")}:
#'  returns the value of \code{N}}
#'  \item{\code{generate.population}}{\code{signature=(object = "Population.Description")}: generates a single realisation of the population.}
#' }
#' @keywords classes
#' @export
#' @seealso \code{\link{make.population.description}}
setClass("Population.Description", representation(N            = "numeric",
                                                  density      = "Density",
                                                  region.name  = "character",
                                                  strata.names = "character",
                                                  covariates   = "list",
                                                  size         = "logical",
                                                  gen.by.N     = "logical"))
#' @importFrom methods validObject is
#' @importFrom dssd make.region
setMethod(
  f="initialize",
  signature="Population.Description",
  definition=function(.Object, N = numeric(0), density = make.density(), region.obj = make.region(), covariates = list(), gen.by.N = TRUE, D.dist = character(0)){
    #Input pre-processing
    # if(!gen.by.N){
    #   ave.density <- NULL
    #   #Calculate average density for each strata.
    #   for(strat in seq(along = density@density.surface)){
    #     ave.density[strat] <- get.ave.density(density.surface = density@density.surface[[strat]], coords = region.obj@coords[[strat]], gaps = region.obj@gaps[[strat]], x.space = density@x.space, y.space = density@y.space)
    #   }
    #   N <- region.obj@area*ave.density
    # }
    # Get the number of strata
    no.strata <- ifelse(length(region.obj@strata.name) > 0, length(region.obj@strata.name), 1)
    # Check covariate input
    covariates <- check.covariates(covariates, no.strata)
    # Check population size input
    if(gen.by.N){
      if(length(N) == 0){
        N <- rep(1000, no.strata)
      }else if(length(N) != no.strata){
        stop("You have not supplied the correct number of constants for population size N (one for each strata).", call. = FALSE)
      }
    }
    # Check if there are clusters
    cov.names <- names(covariates)
    size <- "size" %in% cov.names
    #Set slots
    .Object@N            <- N
    .Object@density      <- density
    .Object@region.name  <- region.obj@region.name
    .Object@strata.names <- region.obj@strata.name
    .Object@covariates   <- covariates
    .Object@size         <- size
    .Object@gen.by.N     <- gen.by.N
    #Check object is valid
    valid <- validObject(.Object, test = TRUE)
    if(is(valid, "character")){
      stop(paste(valid), call. = FALSE)
    }
    # return object
    return(.Object)
  }
)
setValidity("Population.Description",
  function(object){
    if(length(object@N) > 0 & sum(object@N) <= 0){
      return("You must provide a positive, non-zero abundance")
    }
    return(TRUE)
  }
)

# GENERIC METHODS DEFINITIONS --------------------------------------------

#' S4 generic method to return N
#'
#' Returns the population size
#'
#' @param object an object of class Population.Description
#' @return numeric value of the population size
#' @export
#' @rdname get.N-methods
setGeneric("get.N",function(object){standardGeneric ("get.N")})

#' @rdname get.N-methods
#' @export
setMethod("get.N","Population.Description",
          function(object){
            return(object@N)
          }
)

#' @rdname generate.population-methods
#' @param detectability object of class Detectability (optional - only
#'   required if object is of class Population.Description)
#' @param region the region object for the population (optional - only
#'   required if object is of class Population.Description)
#' @export
#' @importFrom methods new is
setMethod(
  f="generate.population",
  signature="Population.Description",
  definition=function(object, detectability = NULL, region = NULL){
    #If the user has not passed in the region object
    if(!is(region, "Region")){
      stop("region must be of class 'Region'")
    }
    if(!is(detectability, "Detectability")){
      stop("detectability must be of class 'Detectability'")
    }
    #If the population has fixed N
    if(object@gen.by.N){
      all.pop.locations <- generate.pop.N(object, region)
    }else{
      all.pop.locations <- generate.pop.D(object, region)

    }
    N <- nrow(all.pop.locations)
    # Make population data.frame
    population.dataframe <- cbind(individual = seq_along(all.pop.locations$x), all.pop.locations)
    # Add covariate values
    if(length(object@covariates) > 0){
      population.dataframe <- add.covariate.values(population.dataframe, object@covariates)
    }
    # Add scale parameter values
    if(N > 0){
      population.dataframe <- calculate.scale.param(population.dataframe,
                                                    detectability, region)
    }
    # Make population object
    population <- new(Class = "Population", region = object@region.name, strata.names = region@region.name, N = N, D = N/region@area, population = population.dataframe, detectability = detectability)
    return(population)
  }
)


