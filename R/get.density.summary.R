get.density.summary <- function(density.surface, dimx, dimy, region){
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
  for(strat in seq(along = density@density.surface)){
    cells <- lapply(1:nrow(density@density.surface[[strat]]),
                    FUN = create.cell,
                    x = density@density.surface[[strat]]$x,
                    y = density@density.surface[[strat]]$y,
                    dimx = dimx,
                    dimy = dimy)
    cells.c <- cells[[1]]
    for(i in seq(along = cells)[-1]){
      cells.c <- c(cells.c, cells[[i]])
    }
    sfc <- sf::st_sfc(cells.c)
    sf.shape = sf::st_sf(density = density@density.surface[[strat]]$density,
                         geom = sfc)
    # Clip each grid cell the strata boundaries
    intersection <- suppressWarnings(sf::st_intersection(sf.shape, region@region[strat,]))
    sf.grids[[strat]] <- intersection
    # Find the areas and densities of each grid cell
    areas <- sf::st_area(intersection)
    # Obtain from sf shape as some grid cells may have been removed
    densities <- intersection$density
    # Find average abundance per grid cell
    N <- areas*densities
    # Create strata summary data
    tmp.data <- data.frame(strata = density@strata.name[strat],
                           area = sf::st_area(region@region[strat,]),
                           ave.N = sum(N),
                           ave.D = sum(N)/sf::st_area(region@region[strat,]))
    if(strat == 1){
      density.summary <- tmp.data
    }else{
      density.summary <- rbind(density.summary, tmp.data)
    }
  }
  # Create a single density grid over the whole study area
  sf.all <- sf.grids[[1]]
  if(length(sf.grids) > 1){
    for(strat in seq(along = sf.grids)[-1]){
      sf.all <- rbind(sf.all, sf.grids[[strat]])
    }
  }
  return(list(summary = density.summary, sf.grid = sf.all))
}
