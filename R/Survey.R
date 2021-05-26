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
#' @return Generate 4 plots showing the survey population, transects (including covered areas), detections and a histogram of the detection distances. Plots inlude the survey region. Also invisibly returns a list of ggplot objects if the user would like to customise the plots.
#' @rdname plot-methods
#' @export
#' @importFrom graphics par
#' @importFrom grDevices nclass.Sturges
#' @importFrom gridExtra grid.arrange
#' @importFrom ggplot2 ggplot geom_sf theme_set theme_bw aes geom_histogram xlim theme_classic labs theme_void
#' @importFrom sf st_as_sf
#' @importFrom rlang .data
setMethod(
  f="plot",
  signature=c("Survey", "Region"),
  definition=function(x, y, ...){
    # Set up plot parameters
    # 4 plot for survey
    suppressWarnings(invisible(gc()))
    p <- list()

    sf.region <- y@region
    transects <- x@transect@samplers

    p[[1]] <- ggplot() + theme_void() +
      geom_sf(data = sf.region, color = gray(.2), lwd = 0.1) +
      geom_sf(data = transects, mapping = aes(), colour = "blue") +
      ggtitle("Transects")

    pop.df <- x@population@population
    pts <- sp::SpatialPoints(data.frame(x = pop.df$x, y = pop.df$y))
    pts.sf <- sf::st_as_sf(pts)
    sf::st_crs(pts.sf) <- sf::st_crs(sf.region)

    p[[2]] <- ggplot() + theme_void() +
      geom_sf(data = sf.region, color = gray(.2), lwd = 0.1, fill = "lightgrey") +
      geom_sf(data = pts.sf, mapping = aes(), colour = "red", cex = 0.5) +
      ggtitle("Population")

    distdata <- na.omit(x@dist.data)
    if(nrow(distdata) > 0){
      pts2 <- sp::SpatialPoints(data.frame(x = distdata$x, y = distdata$y))
      detect.sf <- sf::st_as_sf(pts2)
      sf::st_crs(detect.sf) <- sf::st_crs(sf.region)

      p[[3]] <- ggplot() + theme_void() +
        geom_sf(data = sf.region, color = gray(.2), lwd = 0.1) +
        geom_sf(data = transects, mapping = aes(), colour = "blue") +
        geom_sf(data = pts.sf, mapping = aes(), colour = "red", cex = 0.5) +
        geom_sf(data = detect.sf, mapping = aes(), colour = "cyan", cex = 1) +
        ggtitle("Detections")

      bins <- nclass.Sturges(distdata$distance)
      breaks <- seq(0, max(na.omit(distdata$distance)), length = bins)
      p[[4]] <- ggplot(data=distdata, aes(x = .data$distance)) +
        theme_classic() +
        geom_histogram(breaks=breaks,
                       col="black",
                       fill="grey",
                       alpha = .2) +
        labs(title="Detection Distances", x="distance")

    }else{
      warning("No detections", immediate. = TRUE, call. = FALSE)
      p[[3]] <- ggplot() + theme_void() +
        geom_sf(data = sf.region, color = gray(.2), lwd = 0.1) +
        geom_sf(data = transects, mapping = aes(), colour = "blue") +
        geom_sf(data = pts.sf, mapping = aes(), colour = "red", cex = 0.5) +
        ggtitle("Detections")
    }

    gridExtra::grid.arrange(grobs=p)
    invisible(p)
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
#' @return Generate 2 plots showing the survey population, transects (including covered areas), detections and a histogram of the detection distances. Plots do not include survey region. Also invisibly returns a list of ggplot objects if the user would like to customise the plots.
#' @rdname plot-methods
#' @export
#' @importFrom graphics par
#' @importFrom grDevices nclass.Sturges
#' @importFrom gridExtra grid.arrange
#' @importFrom ggplot2 ggplot geom_sf theme_set theme_bw aes geom_histogram xlim theme_classic labs theme_void
#' @importFrom sf st_as_sf
#' @importFrom rlang .data
setMethod(
  f="plot",
  signature=c("Survey"),
  definition=function(x, y = NULL, ...){
    suppressWarnings(invisible(gc()))
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
    if(nrow(distdata) > 0){
      pts2 <- sp::SpatialPoints(data.frame(x = distdata$x, y = distdata$y))
      detect.sf <- sf::st_as_sf(pts2)
      sf::st_crs(detect.sf) <- sf::st_crs(transects)

      p[[1]] <- ggplot() + theme_void() +
        geom_sf(data = covered.areas, color = gray(.2), lwd = 0.1) +
        geom_sf(data = transects, mapping = aes(), colour = "blue") +
        geom_sf(data = pts.sf, mapping = aes(), colour = "red", cex = 0.5) +
        geom_sf(data = detect.sf, mapping = aes(), colour = "cyan", cex = 1) +
        ggtitle("Example survey")

      bins <- nclass.Sturges(distdata$distance)
      breaks <- seq(0, max(distdata$distance), length = bins)

      p[[2]] <- ggplot(data=distdata, aes(x = .data$distance)) +
        theme_classic() +
        geom_histogram(breaks=breaks,
                       col="black",
                       fill="grey",
                       alpha = .2) +
        labs(title="Detection Distances", x="distance")
    }else{
      warning("No detections", immediate. = TRUE, call. = FALSE)
      p[[1]] <- ggplot() + theme_void() +
        geom_sf(data = covered.areas, color = gray(.2), lwd = 0.1) +
        geom_sf(data = transects, mapping = aes(), colour = "blue") +
        geom_sf(data = pts.sf, mapping = aes(), colour = "red", cex = 0.5) +
        ggtitle("Example survey")
    }


    gridExtra::grid.arrange(grobs=p)

    invisible(p)
  }
)


