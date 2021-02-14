#' @title  Class "Density.Summary"
#' @description  Class \code{"Density.Summary"} is an S4 class containing a
#' summary of the density grids for each strata.
#' @name Density.Summary-class
#' @docType class
#' @slot summary a summary of the average abundances and densitites for
#' each strata.
#' @slot sf.grid a list containing an sf grid with the densities across the
#' study region
#' @keywords classes
#' @seealso \code{\link{make.density}}
#' @export
setClass("Density.Summary", representation(summary = "data.frame"))

#' @importFrom methods validObject
setMethod(
  f="initialize",
  signature="Density.Summary",
  definition=function(.Object, summary){
    #Set slots
    .Object@summary <- summary
    #Check object is valid
    valid <- validObject(.Object, test = TRUE)
    if(class(valid) == "character"){
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

#' @rdname show-methods
#' @export
setMethod("show","Density.Summary",
          function(object){
            print(object@summary)
            invisible(object)
          }
)


