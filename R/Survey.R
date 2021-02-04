#' @include Population.R
#' @include generic.functions.R

#' @title Virtual Class "Survey"
#'
#' @description Class \code{"Survey"} is an S4 class containing an instance of a population.
#'
#' @name Survey-class
#' @slot population Object of class \code{"Population"}; an instance of
#' a population.
#' @keywords classes
setClass("Survey", representation(population = "Population",
                                  dist.data = "data.frame",
                                  dists.in.covered = "numeric", "VIRTUAL"))

# GENERIC METHODS DEFINITIONS --------------------------------------------

#' @export
setMethod(
  f="plot",
  signature=c("Survey"),
  definition=function(x, y = NULL, ...){
    args <- list(...)
    if(!"region" %in% names(args)){
      stop("The region must be supplied in order to plot the survey.", call. = FALSE)
    }
    region <- args$region
    orig.opts <- par(mfrow = c(2,2))
    on.exit(par(orig.opts))
    # Plot transects
    plot(region, x@transect, main = "Transects")
    # Plot population
    plot(region, main = "Population")
    pop <- x@population@population
    points(pop$x, pop$y, pch = 20, col = 2)
    # Plot detections
    plot(region, x@transect, main = "Survey")
    pop <- x@population@population
    points(pop$x, pop$y, pch = 20, col = 1)
    dist.data <- x@dist.data
    ids <- na.omit(dist.data$individual)
    detects <- pop[pop$individual %in% ids,]
    points(detects$x, detects$y, pch = 20, cex = 1.5, col = 6)
    # Plot histogram of detections
    hist(dist.data$distance, main = "Detection Distances", xlab = "Distance from the transect")
    return(invisible(x))
  }
)


