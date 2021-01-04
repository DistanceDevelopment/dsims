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
  definition=function(object, dht.tables = FALSE, ...){
    population <- object@population
    line.transect <- object@transect
    # Find possible detection distances
    poss.distances <- calc.perp.dists(population, line.transect)
    # Record how many were in the covered region
    n.in.covered <- nrow(poss.distances)
    # Simulate detections
    dist.data <- simulate.detections(poss.distances, object@population@detectability)
    # Get the covariate names
    all.col.names <- names(object@population@population)
    cov.param.names <- all.col.names[!all.col.names %in% c("object", "x", "y", "Region.Label", "Sample.Label", "scale.param", "shape.param", "individual")]
    dist.data <- dist.data[,c("object", "individual", "Region.Label", "Sample.Label", "distance", "x", "y", cov.param.names)]
    #ddf.data.obj <- new(Class = "Single.Obs.DDF.Data", data = dist.data)
    # if(dht.tables){
    #   region.table <- create.region.table(object, ...)
    #   sample.table <- create.sample.table(object)
    #   obs.table <- data.frame(object = dist.data$object, Sample.Label = dist.data$transect.ID)
    #   obs.table <- merge(obs.table, sample.table@sample.table, by = "Sample.Label")[,c("object","Sample.Label","Region.Label")]
    #   obs.table.obj <- new(Class = "Obs.Table", data = obs.table)
    #   return(list(ddf.data = ddf.data.obj, obs.table = obs.table.obj, sample.table = sample.table, region.table = region.table, n.in.covered = n.in.covered))
    # }else{
    #   return(list(ddf.data = ddf.data.obj, n.in.covered = n.in.covered))
    # }
    return(list(dist.data = dist.data, n.in.covered = n.in.covered))
  }
)



