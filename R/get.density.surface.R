#' @importFrom sp Polygon Polygons SpatialPolygons
#' @importFrom sf st_crs st_make_grid
#' @importFrom mgcv predict.gam
get.density.surface <- function(region, x.space, y.space, constant = numeric(0), model.fit = NULL, grid.formula = character(0)){
  #Creates a density surface with a constant value across the whole survey region
  # Arguments:
  #   - region   - object of class Region
  #   - x.space  - spacing in the x direction
  #   - y.space  - spacing in the y direction
  #   - constant - a constant value
  #   - model.fit  - an mgcv gam model fitted to x and y explanatory variables
  #   - formula  - a formula with x and y as variables
  # Returns:
  #   - an sf object of a grid of polygons with vector values for
  #     strata, x, y, density

  # For each strata
  strata.names <- region@strata.name
  temp.region <- region@region
  #Need to remove crs as going to work with sp objects
  sf::st_crs(temp.region) <- NA
  # For each strata
  for(i in seq(along = strata.names)){
    # Create a rectangular grid over the entire region
    strata <- temp.region[i,]
    grid.cells = sf::st_make_grid(strata,
                                  cellsize = c(x.space, y.space))
    # Get the bottom left corner points - do before clipping so know there are 5 rows per cell!
    grid.coords <- sf::st_coordinates(grid.cells)
    index <- seq(1, nrow(grid.coords), by = 5)
    grid.coords <- grid.coords[index,]
    # Add data columns
    density.constant <- ifelse(length(constant) > 0, constant[i], NA)
    data.grid <- sf::st_as_sf(data.frame(strata = rep(strata.names[i],length(grid.cells)),
                                         density = rep(density.constant, length(grid.cells)),
                                         x = grid.coords[,"X"] + x.space/2,
                                         y = grid.coords[,"Y"] + y.space/2,
                                         geometry=grid.cells))
    # Clip grid to strata
    suppressWarnings(strata.grid <- sf::st_intersection(data.grid, strata))
    # Remove additional column
    sf.column <- attr(strata.grid, "sf_column")
    strata.grid <- strata.grid[,c("strata", "density", "x", "y", sf.column)]
    if(i == 1){
      region.grid <- strata.grid
    }else{
      region.grid <- rbind(region.grid, strata.grid)
    }
  }
  # Calculate the density values
  if(!is.null(model.fit)){
    # Predict from a model
    pred.data <- data.frame(x = region.grid$x, y = region.grid$y)
    pred.density <- mgcv::predict.gam(model.fit, newdata = pred.data, type = "response")
    region.grid$density <- pred.density
  }else if(length(grid.formula) > 0){
    # Determine from a formula
    x <- region.grid$x
    y <- region.grid$y
    region.grid$density <- eval(parse(text = grid.formula))
  }
  # Match CRS data to region
  sf::st_crs(region.grid) <- sf::st_crs(region@region)
  # Return the grid
  return(region.grid)
}
