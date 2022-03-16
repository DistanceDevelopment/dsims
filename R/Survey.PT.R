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

#' @importFrom methods validObject is
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
    if(is(valid, "character")){
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

#' @rdname run.survey-methods
#' @export
setMethod(
  f="run.survey",
  signature="Survey.PT",
  definition=function(object, region = NULL){
    population <- object@population
    point.transect <- object@transect
    # Find possible detection distances
    poss.distances <- calc.rad.dists(population, point.transect)
    if(!is.null(poss.distances$distance)){
      object@dists.in.covered <- poss.distances$distance
    }
    # Simulate detections
    dist.data <- simulate.detections(poss.distances, object@population@detectability)
    # Check if there are any detections
    if(nrow(dist.data) == 0){
      warning("No detections", immediate. = TRUE, call. = FALSE)
      return(object)
    }
    # Get the covariate names
    all.col.names <- names(object@population@population)
    cov.param.names <- all.col.names[!all.col.names %in% c("object", "x", "y", "Region.Label", "Sample.Label", "scale.param", "shape.param", "individual")]
    dist.data <- dist.data[,c("object", "individual", "Region.Label", "Sample.Label", "distance", "x", "y", cov.param.names)]
    # Add in the transect lengths
    sample.table <- data.frame(Region.Label = point.transect@samplers$strata,
                               Sample.Label = point.transect@samplers$transect,
                               Effort = rep(1,length(point.transect@samplers$transect)))
    # If the region is supplied then add in the survey region Area
    if(!is.null(region)){
      region.table <- data.frame(Region.Label = region@strata.name,
                                 Area = region@area)
      sample.table <- dplyr::left_join(sample.table, region.table, by = "Region.Label")
    }
    dist.data <- dplyr::full_join(dist.data, sample.table, by = c("Sample.Label", "Region.Label"))
    # Order by transect id
    index <- order(dist.data$Sample.Label)
    dist.data <- dist.data[index,]
    object@dist.data <- dist.data
    object@dists.in.covered <- poss.distances$distance
    return(object)
  }
)


