#' @include Detectability.R

#' @title Class "Population" 
#' 
#' @description Contains an instance of a population including a description of their detectability
#' in the form of an object of class Detectability.
#'
#' @name Population-class
#' @title S4 Class "Population"
#' @slot region.obj Object of class \code{"character"}; the name of the region
#'  object.
#' @slot strata.names Object of class \code{"character"}; the names of the 
#'  strata.
#' @slot N Object of class \code{"numeric"}; the number of individuals/clusters.
#' @slot D Object of class \code{"numeric"}; the density of individuals/clusters.
#' @slot population  Object of class \code{"data.frame"}; the locations of 
#'  individuals/clusters and any population covariates.
#' @slot detectability  Object of class \code{"Detectability"}; describes how
#'  easily the individuals/clusters can be detected.
#' @section Methods:
#' \describe{
#'  \item{\code{plot}}{\code{signature=(object = "Line.Transect")}: plots the locations
#'  of the individuals/clusters.}
#' }
#' @keywords classes
#' @seealso \code{\link{make.population.description}}, \code{\link{make.detectability}}
setClass("Population", representation(region.obj   = "character",
                                      strata.names = "character", 
                                      N            = "numeric", 
                                      D            = "numeric",
                                      population   = "data.frame",
                                      detectability = "Detectability")) 
setMethod(
  f="initialize",
  signature="Population",
  definition=function(.Object, region, strata.names, N, D, population, detectability){
    #Input pre-processing
    #Set slots
    .Object@region.obj   <- region
    .Object@strata.names <- strata.names
    .Object@D            <- D
    .Object@N            <- N
    .Object@population   <- population
    .Object@detectability <- detectability
    #Check object is valid
    validObject(.Object)
    # return object
    return(.Object) 
  }
)
setValidity("Population",
  function(object){
    return(TRUE)
  }
)

# GENERIC METHODS DEFINITIONS --------------------------------------------

#' Plot
#' 
#' Plots an S4 object of class 'Population'. Requires that the
#' associated region has already been plotted. This function adds 
#' the locations of the individuals/clusters in the population.
#' 
#' @param x object of class Population
#' @param y not used
#' @param ... other general plot parameters 
#' @rdname plot.Population-methods
#' @importFrom graphics points
#' @exportMethod plot
setMethod(
  f="plot",
  signature="Population",
  definition=function(x, y, ...){
    points(x@population$x, x@population$y, col = 2, pch = 20, ...) 
    invisible(x)
  }    
) 





