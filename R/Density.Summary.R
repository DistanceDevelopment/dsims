#' @title  Class "Density.Summary"
#' @description  Class \code{"Density.Summary"} is an S4 class containing a
#' summary of the density grids for each strata.
#' @name Density.Summary-class
#' @docType class
#' @slot summary a summary of the average abundances and densities for
#' each strata.
#' @keywords classes
#' @seealso \code{\link{make.density}}
#' @export
setClass("Density.Summary", representation(summary = "data.frame"))

#' @importFrom methods validObject is
setMethod(
  f="initialize",
  signature="Density.Summary",
  definition=function(.Object, summary){
    #Set slots
    .Object@summary <- summary
    #Check object is valid
    valid <- validObject(.Object, test = TRUE)
    if(is(valid, "character")){
      stop(paste(valid), call. = FALSE)
    }
    # return object
    return(.Object)
  }
)
setValidity("Density.Summary",
            function(object){
              return(TRUE)
            }
)

# GENERIC METHODS DEFINITIONS --------------------------------------------

#' show
#'
#' displays the density summary table
#'
#' @param object object of class Density.Summary
#' @return No return value, displays the density summary
#' @rdname show.Density.Summary-methods
#' @export
setMethod("show","Density.Summary",
          function(object){
            print(object@summary)
          }
)


