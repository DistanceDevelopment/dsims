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
  signature=c("Survey", "Region"),
  definition=function(x, y, ...){
    # Set up plot parameters
    args <- list(...)

    if("strata.col" %in% names(args)){
      strata.col <- args$strata.col
      if(length(strata.col) <- length(y@strata.name)){
        multiply <- ceiling(length(y@strata.name)/length(strata.col))
        strata.col <- rep(strata.col, multiply)
      }
    }else{
      strata.col <- rep("ivory1", length(y@strata.name))
    }
    transect.col <- ifelse("transect.col" %in% names(args), args$transect.col, 4)
    population.col <- ifelse("population.col" %in% names(args), args$population.col, 2)
    detect.col <- ifelse("detect.col" %in% names(args), args$detect.col, "cyan")
    cov.areas <- ifelse("covered.areas" %in% names(args), args$covered.areas, FALSE)
    lwd <- ifelse("lwd" %in% names(args), args$lwd, 1)
    # Region info
    region <- y
    sf.region <- region@region
    sf.column <- attr(sf.region, "sf_column")
    bbox <- sf::st_bbox(sf.region)
    if(length(region@units) > 0){
      x.label <- paste("x-coords (", region@units, ")", sep = "")
      y.label <- paste("y-coords (", region@units, ")", sep = "")
    }else{
      x.label <- "x.coordinates"
      y.label <- "y.coordinates"
    }
    # Samplers info
    samps <- x@transect
    sf.column.samps <- attr(samps@samplers, "sf_column")
    bbox.samps <- sf::st_bbox(samps@samplers)
    # Population info
    pop <- x@population@population
    # Plot format
    orig.opts <- par(mfrow = c(2,2))
    on.exit(par(orig.opts))
    # Plot 1 - region and transects
    # plot region
    plot(c(0,0), col = "white",
         xlim = c(min(bbox$xmin,bbox.samps$xmin), max(bbox$xmax,bbox.samps$xmax)),
         ylim = c(min(bbox$ymin,bbox.samps$ymin), max(bbox$ymax,bbox.samps$ymax)),
         main = "Transects", xlab = x.label, ylab = y.label)
    for(i in seq(along = sf.region[[sf.column]])){
      plot(sf.region[[sf.column]][[i]], add = TRUE, col = strata.col[i])
    }
    # plot transects
    plot(samps@samplers[[sf.column.samps]], col = transect.col, lwd = lwd, add = TRUE)

    # Plot 2 - region and population
    # plot region
    plot(c(0,0), col = "white",
         xlim = c(bbox$xmin, bbox$xmax), ylim = c(bbox$ymin, bbox$ymax),
         main = "Population", xlab = x.label, ylab = y.label)
    for(i in seq(along = sf.region[[sf.column]])){
      plot(sf.region[[sf.column]][[i]], add = TRUE, col = strata.col[i])
    }
    # plot population
    points(pop$x, pop$y, pch = 20, col = population.col)

    # Plot 3 - region, population and detections (covered areas optional)
    plot(c(0,0), col = "white",
         xlim = c(min(bbox$xmin,bbox.samps$xmin), max(bbox$xmax,bbox.samps$xmax)),
         ylim = c(min(bbox$ymin,bbox.samps$ymin), max(bbox$ymax,bbox.samps$ymax)),
         main = "Survey", xlab = x.label, ylab = y.label)
    for(i in seq(along = sf.region[[sf.column]])){
      plot(sf.region[[sf.column]][[i]], add = TRUE, col = strata.col[i])
    }
    # plot covered areas
    if(cov.areas){
      cov.area.shape <- x@transect@cov.area.polys
      sf.col.ca <- attr(cov.area.shape, "sf_column")
      cov.area.shape <- cov.area.shape[[sf.col.ca]]
      for(i in seq(along = cov.area.shape)){
        plot(cov.area.shape[[i]], add = T)
      }
    }
    # plot transects
    plot(samps@samplers[[sf.column.samps]], col = transect.col, lwd = lwd, add = TRUE)
    # plot population
    points(pop$x, pop$y, pch = 20, col = population.col)
    # add detections
    points(x@dist.data$x, x@dist.data$y, pch = 20, col = detect.col)

    # Plot 4 - histogram of detection distances
    hist.xlab <- ifelse(class(x@transect) == "Line.Transect", "perpendicular distance", "radial distance")
    if(length(region@units) > 0){
      hist.xlab <- paste(hist.xlab, " (", region@units, ")", sep = "")
    }
    hist(x@dist.data$distance, main = "Detection Distances", xlab = hist.xlab)

    return(invisible(x))
  }
)


#' @export
setMethod(
  f="plot",
  signature=c("Survey"),
  definition=function(x, y = NULL, ...){
    # Set up plot parameters
    args <- list(...)

    transect.col <- ifelse("transect.col" %in% names(args), args$transect.col, 4)
    population.col <- ifelse("population.col" %in% names(args), args$population.col, 2)
    detect.col <- ifelse("detect.col" %in% names(args), args$detect.col, "cyan")
    cov.areas <- ifelse("covered.areas" %in% names(args), args$covered.areas, TRUE)
    lwd <- ifelse("lwd" %in% names(args), args$lwd, 1)
    # Samplers info
    samps <- x@transect
    sf.column.samps <- attr(samps@samplers, "sf_column")
    bbox.samps <- sf::st_bbox(samps@cov.area.polys)
    # Population info
    pop <- x@population@population
    x.range <- range(pop$x)
    y.range <- range(pop$y)
    # Plot format
    orig.opts <- par(mfrow = c(1,2))
    on.exit(par(orig.opts))
    # Plot 1 - region and transects
    # plot region
    plot(c(0,0), col = "white",
         xlim = c(min(bbox.samps$xmin, x.range[1]), max(bbox.samps$xmax,x.range[2])),
         ylim = c(min(bbox.samps$ymin,y.range[1]), max(bbox.samps$ymax,y.range[2])),
         main = "Survey", xlab = "x.coordinates", ylab = "y.coordinate")
    # plot covered areas
    if(cov.areas){
      cov.area.shape <- x@transect@cov.area.polys
      sf.col.ca <- attr(cov.area.shape, "sf_column")
      cov.area.shape <- cov.area.shape[[sf.col.ca]]
      for(i in seq(along = cov.area.shape)){
        plot(cov.area.shape[[i]], add = T)
      }
    }
    # plot transects
    plot(samps@samplers[[sf.column.samps]], col = transect.col, lwd = lwd, add = TRUE)
    # plot population
    points(pop$x, pop$y, pch = 20, col = population.col)
    # add detections
    points(x@dist.data$x, x@dist.data$y, pch = 20, col = detect.col)

    # Plot 4 - histogram of detection distances
    hist.xlab <- ifelse(class(x@transect) == "Line.Transect", "perpendicular distance", "radial distance")
    if(length(region@units) > 0){
      hist.xlab <- paste(hist.xlab, " (", region@units, ")", sep = "")
    }
    hist(x@dist.data$distance, main = "Detection Distances", xlab = hist.xlab)

    return(invisible(x))
  }
)


