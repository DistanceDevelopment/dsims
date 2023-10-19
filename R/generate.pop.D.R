#' @importFrom stats rpois runif
#' @importFrom sf as_Spatial st_coordinates st_as_sf st_intersection
generate.pop.D <- function(population.description, region){
#this function generates a population based on the values in the
#Density object grid (not from a fixed population size)
  density <- population.description@density
  density.sf <- density@density.surface[[1]]
  first <- TRUE
  #Extract region and remove crs as we will work with spatial shapes
  strata.names <- region@strata.name
  temp.region <- region@region
  sf::st_crs(temp.region) <- NA
  sf.column <- attr(temp.region, "sf_column")
  # the abundances for the grid cells are generated as if the whole
  # grid cell were inside the study area, only the animals inside the
  # study rea will then be retained.
  cell.area <- density@x.space*density@y.space
  # Generate the population strata by strata as sometimes the same grid cell
  # contains 2 strata and in this case is replicated
  for(strat in seq(along = strata.names)){
    # get the cell densities
    density.sf.strata <- density.sf[density.sf$strata == strata.names[strat],]
    densities <- density.sf.strata$density
    n.cells <- length(densities)
    # Calculate the cell abundances from a Poisson distribution
    lambdas <- densities*cell.area
    no.in.cells <- rpois(n.cells,  lambda = lambdas)
    if(sum(no.in.cells) > 0){
      #repeat each row count the number of times in no.in.cell
      row.ids <- rep(1:n.cells, no.in.cells)
      #extract grid cells
      grid.locations <- density.sf.strata[row.ids,c("x","y")]
      #generate random locations within grid cell
      rx <- runif(nrow(grid.locations), -density@x.space/2, density@x.space/2)
      ry <- runif(nrow(grid.locations), -density@y.space/2, density@y.space/2)
      # Put x,y coords of animals in a data frame
      pts <- data.frame(x = grid.locations$x+rx, y = grid.locations$y+ry)
      # Make it an sf object
      pts.sf <- sf::st_as_sf(pts, coords = c("x", "y"))
      inside <- suppressWarnings(sf::st_intersection(pts.sf, temp.region[strat,]))
      # Extract the coordinates
      pop.locations <- as.data.frame(st_coordinates(inside))
      names(pop.locations) <- c("x","y")
      # Record strata ID
      pop.locations$Region.Label <- strata.names[strat]
      if(strat == 1){
        all.pop.locations <- pop.locations
      }else{
        all.pop.locations <- rbind(all.pop.locations, pop.locations)
      }
    }
  }
  return(all.pop.locations)
}


