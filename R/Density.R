#' @include generic.functions.R
#' @include Density.Summary.R

#' @title  Class "Density"
#' @description  Class \code{"Density"} is an S4 class containing a list of grids which
#' describe the density of individuals / clusters of a population. The list
#' contains one grid (\code{data.frame}) for each strata.
#' @name Density-class
#' @docType class
#' @slot region.name Object of class \code{"character"}; the region name.
#' @slot strata.name Object of class \code{"character"}; the strata names
#' @slot density.surface Object of class \code{"list"}; list of data.frames
#' with the columns x, y and density. There must be one data.frame for each
#' strata.
#' @slot x.space Object of class \code{"numeric"}; The spacing between
#' gridpoints described in the density data.frames in the x-direction.
#' @slot y.space Object of class \code{"numeric"}; The spacing between
#' gridpoints described in the density data.frames in the y-direction.
#' @slot units Object of class \code{"numeric"}; The units of the grid
#' points.
#' @keywords classes
#' @seealso \code{\link{make.density}}
#' @export
setClass("Density", representation(region.name = "character",
                                   strata.name = "character",
                                   density.surface = "list",
                                   x.space = "numeric",
                                   y.space = "numeric",
                                   units = "character"))

#' @importFrom methods validObject
setMethod(
  f="initialize",
  signature="Density",
  definition=function(.Object, region, strata.name = character(0), density.surface = list(), x.space, y.space, constant = NULL, model.fit = NULL, density.formula = character(0)){
    # Create density surface
    if(length(density.surface) == 0){
      density.surface <- get.density.surface(region = region,
                                             x.space = x.space,
                                             y.space = y.space,
                                             constant = constant,
                                             model.fit = model.fit,
                                             grid.formula = density.formula)
    }
    #Set slots
    .Object@region.name <- region@region.name
    .Object@strata.name <- strata.name
    .Object@density.surface <- list(density.surface)
    .Object@x.space <- x.space
    .Object@y.space <- y.space
    .Object@units <- region@units
    #Check object is valid
    valid <- validObject(.Object, test = TRUE)
    if(class(valid) == "character"){
      stop(paste(valid), call. = FALSE)
    }
    # return object
    return(.Object)
  }
)
setValidity("Density",
  function(object){
    #check region object exists and is of the correct class
    #check strata object exists and is of the correct class
    #check the density grid was created without problem
    some.strata.with.grids <- FALSE
    some.strata.with.no.grids <- FALSE
    density.sf <- object@density.surface[[1]]
    strata.names <- object@strata.name
    for(i in seq(along = object@density.surface)){
      # Get densities for current strata
      densities <- density.sf$density[density.sf$strata == strata.names[i]]
      # Check if there are any negative values for density
      if(any(densities < 0)){
        return("All density values must be positive!")
      }
      density.sum <- sum(densities)
      #check there are some cells with non-zero density
      if(density.sum == 0){
        return("All strata must have some cells with non-zero density. Check that you have correctly specified your density grid. Large grid spacing may also generate this error.")
      }
      if(length(densities) > 0){
        some.strata.with.grids <- TRUE
      }else{
        some.strata.with.no.grids <- TRUE
      }
    }
    if(some.strata.with.grids & some.strata.with.no.grids){
      return("The grid spacing needs to be smaller, not all strata have points in them")
    }else if(!some.strata.with.grids){
      return("There has been a problem generating the density grid. You must supply either a valid density surface, constant or valid density gam argument. DSM and formula are not currently suported")
    }
    return(TRUE)
  }
)

# GENERIC METHODS DEFINITIONS --------------------------------------------

#' @rdname add.hotspot-methods
#' @export
setMethod("add.hotspot","Density",
          function(object, centre, sigma, amplitude){
            sf.density.df <- object@density.surface[[1]][,c("x","y","density")]
            # Find distances from centre to each point on the density surface
            dists <- sqrt((sf.density.df$x-centre[1])^2 + (sf.density.df$y-centre[2])^2)
            # Calculate radial decay
            additive.values <- (exp(-dists^2/(2*sigma^2)))*amplitude
            # Add to current density values
            new.densities <- sf.density.df$density + additive.values
            # Put them back in sf object if they are non-negative
            if(any(new.densities < 0)){
              warning("Adding this high / low spot to the density map would have resulted in negative densities. Object returned unchanged.")
              return(object)
            }
            object@density.surface[[1]]$density <- new.densities
            return(object)
          }
)


#' Plot
#'
#' Plots an S4 object of class 'Density'
#'
#' @param x object of class Density
#' @param y not used
#' @param strata the strata name or number to be plotted. By default
#' all strata will be plotted.
#' @param title plot title
#' @return ggplot object
#' @rdname plot.Density-methods
#' @importFrom ggplot2 ggplot geom_sf scale_fill_viridis_c ggtitle aes theme_set theme_bw
#' @importFrom grDevices gray
#' @importFrom stats density
#' @exportMethod plot
setMethod(
  f = "plot",
  signature = c("Density"),
  definition = function(x, y, strata = "all", title = ""){
    # Extract strata names
    strata.names <- x@strata.name
    # Extract plot data
    if(is.character(strata)){
      if(!strata %in% c(x@strata.name, "all")){
        stop("You have provided an unrecognised strata name.", call. = FALSE)
      }
    }
    # set up plot data
    sf.column <- attr(x@density.surface[[1]], "sf_column")
    if(strata == "all"){
      plot.data <- x@density.surface[[1]][,c("density", sf.column)]
      if(title == ""){
        title <- x@region.name
      }
    }else if(is.numeric(strata)){
      plot.data <- x@density.surface[[1]][x@density.surface[[1]]$strata == strata.names[strata],c("density", sf.column)]
      if(title == ""){
        title <- strata.names[strata]
      }
    }else if(is.character(strata)){
      plot.data <- x@density.surface[[1]][x@density.surface[[1]]$strata == strata,c("density", sf.column)]
      if(title == ""){
        title <- strata
      }
    }
    # Set theme
    tmp <- theme_set(theme_bw())
    on.exit(theme_set(tmp))

    # Create the plot object
    ggplot.obj <- ggplot() +
      geom_sf(data = plot.data, mapping = aes(fill = density), lwd = 0) +
      scale_fill_viridis_c() +
      ggtitle(title)

    # return the plot object incase the user wants to modify
    return(ggplot.obj)
  }
)


#' Plot
#'
#' Plots an S4 object of class 'Density'
#'
#' @param x object of class Density
#' @param y object of class Region
#' @param strata the strata name or number to be plotted. By default
#' all strata will be plotted.
#' @param title plot title
#' @return ggplot object
#' @rdname plot.Density-methods
#' @importFrom ggplot2 ggplot geom_sf scale_fill_viridis_c ggtitle aes theme_set theme_bw
#' @importFrom grDevices gray
#' @importFrom stats density
#' @exportMethod plot
setMethod(
  f = "plot",
  signature = c("Density","Region"),
  definition = function(x, y, strata = "all", title = ""){
    # Extract strata names
    strata.names <- x@strata.name
    # Extract plot data
    if(is.character(strata)){
      if(!strata %in% c(x@strata.name, "all")){
        stop("You have provided an unrecognised strata name.", call. = FALSE)
      }
    }
    # set up plot data
    sf.column <- attr(x@density.surface[[1]], "sf_column")
    if(strata == "all"){
      plot.data <- x@density.surface[[1]][,c("density", sf.column)]
      if(title == ""){
        title <- x@region.name
      }
    }else if(is.numeric(strata)){
      plot.data <- x@density.surface[[1]][x@density.surface[[1]]$strata == strata.names[strata],c("density", sf.column)]
      if(title == ""){
        title <- strata.names[strata]
      }
    }else if(is.character(strata)){
      plot.data <- x@density.surface[[1]][x@density.surface[[1]]$strata == strata,c("density", sf.column)]
      if(title == ""){
        title <- strata
      }
    }
    # Set theme
    tmp <- theme_set(theme_bw())
    on.exit(theme_set(tmp))

    # Extract region data
    sf.region <- y@region

    # Create the plot object
    ggplot.obj <- ggplot() +
      geom_sf(data = plot.data, mapping = aes(fill = density), lwd = 0) +
      scale_fill_viridis_c() +
      geom_sf(data = sf.region, fill = NA, color = gray(.2), lwd = 0.2) +
      ggtitle(title)

    # return the plot object incase the user wants to modify
    return(ggplot.obj)
  }
)

#' summary
#'
#' Provides a summary table of the density object.
#'
#' @param object object of class Simulation
#' @param ... not implemented
#' @rdname summary.Density-methods
#' @importFrom stats na.omit qlnorm qnorm
#' @importFrom methods slotNames
#' @importFrom methods new
#' @export
setMethod(
  f = "summary",
  signature = "Density",
  definition = function(object, ...){
    density.sf <- object@density.surface[[1]]
    # Get strata names
    strata.name <- unique(density.sf$strata)
    # For each strata
    for(strat in seq(along = strata.name)){
      # Get sf shapes relevant to current strata
      strat.grid <- density.sf[density.sf$strata == strata.name[strat],]
      # Find the areas and densities of each grid cell
      areas <- sf::st_area(strat.grid)
      # Obtain from sf shape as some grid cells may have been removed
      densities <- strat.grid$density
      # Find average abundance per grid cell
      N <- areas*densities
      # Create strata summary data
      tmp.data <- data.frame(strata = strata.name[strat],
                             area = sum(areas),
                             ave.N = sum(N),
                             ave.D = sum(N)/sum(areas))
      if(strat == 1){
        density.summary <- tmp.data
      }else{
        density.summary <- rbind(density.summary, tmp.data)
      }
    }
    # Create a new Density.Summary object
    density.summary <- new(Class = "Density.Summary",
                           summary = density.summary)
    return(density.summary)
  }
)







