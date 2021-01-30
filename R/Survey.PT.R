#' @include Survey.R

#' @title Class "Survey.PT" extends class "Survey"
#'
#' @description Class \code{"Survey.PT"} is an S4 class containing a population
#' and a set of transects.
#' @name Survey.PT-class
#' @title S4 Class "Survey.PT"
#' @slot transect Object of class \code{"Point.Transect"}; the
#'  point transects.
#' @slot radial.truncation Object of class \code{"numeric"}; the
#'  maximum distance from the transect at which animals may be detected.
#' @keywords classes
#' @importClassesFrom dssd Point.Transect
#' @seealso \code{\link{make.design}}
#' @export
setClass(Class = "Survey.PT",
         representation = representation(transect = "Point.Transect",
                                         radial.truncation = "numeric"),
         contains = "Survey"
)

setMethod(
  f="initialize",
  signature="Survey.PT",
  definition=function(.Object, population, transect, rad.truncation){
    #Input pre-processing
    .Object@population        <- population
    .Object@transect          <- transect
    .Object@radial.truncation <- rad.truncation
    #Check object is valid
    valid <- validObject(.Object, test = TRUE)
    if(class(valid) == "character"){
      stop(paste(valid), call. = FALSE)
    }
    # return object
    return(.Object)
  }
)
setValidity("Survey.PT",
            function(object){
              return(TRUE)
            }
)

# GENERIC METHODS DEFINITIONS --------------------------------------------



