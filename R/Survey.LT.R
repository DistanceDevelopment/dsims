#' @include Survey.R

#' @title Class "Survey.LT" extends class "Survey"
#'
#' @description Class \code{"Survey.LT"} is an S4 class containing a population
#' and a set of transects.
#' @name Survey.LT-class
#' @title S4 Class "Survey.LT"
#' @slot transect Object of class \code{"Line.Transect"}; the
#'  line transects.
#' @slot perpendicular.truncation Object of class \code{"numeric"}; the
#'  maximum distance from the transect at which animals may be detected.
#' @keywords classes
#' @importClassesFrom dssd Line.Transect
#' @seealso \code{\link{make.design}}
#' @export
setClass(Class = "Survey.LT",
         representation = representation(transect = "Line.Transect",
                                         perpendicular.truncation = "numeric"),
         contains = "Survey"
)

setMethod(
  f="initialize",
  signature="Survey.LT",
  definition=function(.Object, population, transect, perp.truncation){
    #Input pre-processing
    .Object@population        <- population
    .Object@transect          <- transect
    .Object@perpendicular.truncation <- perp.truncation
    #Check object is valid
    validObject(.Object)
    # return object
    return(.Object)
  }
)
setValidity("Survey.LT",
            function(object){
              return(TRUE)
            }
)

# GENERIC METHODS DEFINITIONS --------------------------------------------

#' @rdname create.survey.results-methods
#' @export
setMethod(
  f="create.survey.results",
  signature="Survey.LT",
  definition=function(object, region = NULL){
    population <- object@population
    line.transect <- object@transect
    # Find possible detection distances
    poss.distances <- calc.perp.dists(population, line.transect)
    # Simulate detections
    dist.data <- simulate.detections(poss.distances, object@population@detectability)
    # Get the covariate names
    all.col.names <- names(object@population@population)
    cov.param.names <- all.col.names[!all.col.names %in% c("object", "x", "y", "Region.Label", "Sample.Label", "scale.param", "shape.param", "individual")]
    dist.data <- dist.data[,c("object", "individual", "Region.Label", "Sample.Label", "distance", "x", "y", cov.param.names)]
    # Add in the transect lengths
    sample.table <- data.frame(Region.Label = transects@samplers$strata,
                               Sample.Label = transects@samplers$transect,
                               Effort = sf::st_length(transects@samplers))
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
    return(list(dist.data = dist.data, dists.in.covered = poss.distances))
  }
)



