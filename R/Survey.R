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


#' @param x object of class Survey
#' @param y NULL
#' @param ... additional plotting parameters
#' @rdname plot-methods
#' @export
#' @importFrom graphics par
#' @importFrom gridExtra grid.arrange
#' @importFrom ggplot2 ggplot geom_sf theme_set theme_bw aes
#' @importFrom sf st_as_sf
setMethod(
  f="plot",
  signature=c("Survey", "Region"),
  definition=function(x, y, ...){
    # Set up plot parameters
    # 4 plot for survey
    p <- list()

    sf.region <- y@region
    transects <- x@transect@samplers

    p[[1]] <- ggplot() + theme_set(theme_bw()) +
      geom_sf(data = sf.region, color = gray(.2), lwd = 0.1) +
      geom_sf(data = transects, mapping = aes(), colour = "blue") +
      ggtitle("Transects")

    pop.df <- x@population@population
    pts <- sp::SpatialPoints(data.frame(x = pop.df$x, y = pop.df$y))
    pts.sf <- sf::st_as_sf(pts)
    sf::st_crs(pts.sf) <- sf::st_crs(sf.region)

    p[[2]] <- ggplot() + theme_set(theme_bw()) +
      geom_sf(data = sf.region, color = gray(.2), lwd = 0.1, fill = "lightgrey") +
      geom_sf(data = pts.sf, mapping = aes(), colour = "red", cex = 0.5) +
      ggtitle("Population")

    distdata <- na.omit(x@dist.data)
    pts2 <- sp::SpatialPoints(data.frame(x = distdata$x, y = distdata$y))
    detect.sf <- sf::st_as_sf(pts2)
    sf::st_crs(detect.sf) <- sf::st_crs(sf.region)


    p[[3]] <- ggplot() + theme_set(theme_bw()) +
      geom_sf(data = sf.region, color = gray(.2), lwd = 0.1) +
      geom_sf(data = transects, mapping = aes(), colour = "blue") +
      geom_sf(data = pts.sf, mapping = aes(), colour = "red", cex = 0.5) +
      geom_sf(data = detect.sf, mapping = aes(), colour = "cyan", cex = 1) +
      ggtitle("Detections")


    p[[4]] <- ggplot(distdata, aes(x=distance)) + theme_set(theme_bw()) +
      geom_histogram(color="black", fill="lightgrey", bins = nclass.Sturges(distdata$distance)) +
      ggtitle("Detection Distances")

    gridExtra::grid.arrange(grobs=p)

    return(invisible(p))
  }
)

#' plot
#'
#' Produces four plots of the survey: 1) Plots the transects inside the survey
#' region, 2) plots the population, 3) plots the transects, population and
#' detections 4) plots a histogram of the detection distances. Note that only
#' plots 3 & 4 are generated without the survey region if Region is omitted.
#'
#' @param x object of class Survey
#' @param y object of class Region
#' @param ... additional plotting parameters
#' @rdname plot-methods
#' @export
#' @importFrom graphics par
#' @importFrom gridExtra grid.arrange
#' @importFrom ggplot2 ggplot geom_sf theme_set theme_bw aes
#' @importFrom sf st_as_sf
setMethod(
  f="plot",
  signature=c("Survey"),
  definition=function(x, y = NULL, ...){

    p <- list()
    # Set up data
    # Transects
    transects <- x@transect@samplers
    covered.areas <- x@transect@cov.area.polys
    # Population
    pop.df <- x@population@population
    pts <- sp::SpatialPoints(data.frame(x = pop.df$x, y = pop.df$y))
    pts.sf <- sf::st_as_sf(pts)
    sf::st_crs(pts.sf) <- sf::st_crs(transects)
    # Detections
    distdata <- na.omit(x@dist.data)
    pts2 <- sp::SpatialPoints(data.frame(x = distdata$x, y = distdata$y))
    detect.sf <- sf::st_as_sf(pts2)
    sf::st_crs(detect.sf) <- sf::st_crs(transects)

    p[[1]] <- ggplot() + theme_set(theme_bw()) +
      geom_sf(data = covered.areas, color = gray(.2), lwd = 0.1) +
      geom_sf(data = transects, mapping = aes(), colour = "blue") +
      geom_sf(data = pts.sf, mapping = aes(), colour = "red", cex = 0.5) +
      geom_sf(data = detect.sf, mapping = aes(), colour = "cyan", cex = 1) +
      ggtitle("Example survey")

    p[[2]] <- ggplot(distdata, aes(x=distance)) + theme_set(theme_bw()) +
      geom_histogram(color="black", fill="lightgrey", bins = nclass.Sturges(distdata$distance)) +
      ggtitle("Detection Distances")

    gridExtra::grid.arrange(grobs=p)

    return(invisible(p))
  }
)


