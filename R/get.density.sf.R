get.density.sf <- function(density.surface, dimx, dimy, region){
  # Get strata names
  strata.name <- region@strata.name
  # Calculate the average density for the density grids
  create.cell <- function(i, x, y, dimx, dimy){
    # Create square polygons for each grid cell
    p1 = matrix(c((x[i]-dimx/2),(y[i]-dimy/2),
                  (x[i]-dimx/2),(y[i]+dimy/2),
                  (x[i]+dimx/2),(y[i]+dimy/2),
                  (x[i]+dimx/2),(y[i]-dimy/2),
                  (x[i]-dimx/2),(y[i]-dimy/2)),ncol=2, byrow=TRUE)
    pol1 = sf::st_sfc(sf::st_polygon(list(p1)))
    return(pol1)
  }
  sf.grids <- list()
  # For each strata
  for(strat in seq(along = density.surface)){
    cells <- lapply(1:nrow(density.surface[[strat]]),
                    FUN = create.cell,
                    x = density.surface[[strat]]$x,
                    y = density.surface[[strat]]$y,
                    dimx = dimx,
                    dimy = dimy)
    cells.c <- cells[[1]]
    for(i in seq(along = cells)[-1]){
      cells.c <- c(cells.c, cells[[i]])
    }
    sfc <- sf::st_sfc(cells.c)
    sf.shape = sf::st_sf(density = density.surface[[strat]]$density,
                         x = density.surface[[strat]]$x,
                         y = density.surface[[strat]]$y,
                         strata = rep(strata.name[strat], nrow(density.surface[[strat]])),
                         geom = sfc)
    # Clip each grid cell the strata boundaries
    intersection <- suppressWarnings(sf::st_intersection(sf.shape, region@region[strat,]))
    sf.grids[[strat]] <- intersection
  }
  # Create a single density grid over the whole study area
  sf.all <- sf.grids[[1]]
  if(length(sf.grids) > 1){
    for(strat in seq(along = sf.grids)[-1]){
      sf.all <- rbind(sf.all, sf.grids[[strat]])
    }
  }
  return(sf.grid = sf.all)
}
