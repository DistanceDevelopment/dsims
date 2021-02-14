#' @importFrom stats rpois runif
#' @importFrom sf as_Spatial
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
      #find x,y coords of animals
      grid.locations$x.coord <- grid.locations$x+rx
      grid.locations$y.coord <- grid.locations$y+ry
      #find which x,y coords are within the region
      pts <- sp::SpatialPoints(data.frame(x = grid.locations$x.coord,
                                          y = grid.locations$y.coord))
      #Extract shape for current strata
      strata.sp <- sf::as_Spatial(temp.region[strat,][[sf.column]])
      # Find which points are inside the current strata
      inside <- pts[strata.sp,]
      # Extract the coordinates
      pop.locations <- as.data.frame(inside@coords)
      #Record strata ID
      pop.locations$Region.Label <- rep(strata.names[strat], nrow(pop.locations))
      if(strat == 1){
        all.pop.locations <- pop.locations
      }else{
        all.pop.locations <- rbind(all.pop.locations, pop.locations)
      }
    }
  }
  return(all.pop.locations)
}


