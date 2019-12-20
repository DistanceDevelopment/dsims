#' @importFrom stats rpois runif
#' @importFrom splancs as.points
generate.pop.D <- function(population.description, region.obj){
#this function generates a population based on the values in the
#Density object grid (not from a fixed population size)
  density.obj <- population.description@density
  first <- TRUE
  #Get sf column
  sf.column <- attr(region.obj@region, "sf_column")
  for(strat in seq(along = density.obj@density.surface)){
    n.cells <- nrow(density.obj@density.surface[[strat]])
    densities <- density.obj@density.surface[[strat]][["density"]]
    cell.area <- density.obj@x.space*density.obj@y.space
    lambdas <- densities*cell.area
    #generate the number of animals to fall in each cell from a Poisson distribution
    no.in.cells <- rpois(n.cells,  lambda = lambdas)
    #check there are some animals
    if(sum(no.in.cells) > 0){
      #repeat each row count the number of times in no.in.cell
      row.ids <- rep(1:n.cells, no.in.cells)
      #extract grid cells
      grid.locations <- density.obj@density.surface[[strat]][row.ids,]
      #generate random locations within grid cell
      rx <- runif(nrow(grid.locations), -density.obj@x.space/2, density.obj@x.space/2)
      ry <- runif(nrow(grid.locations), -density.obj@y.space/2, density.obj@y.space/2)
      #find x,y coords of animals
      grid.locations$x.coord <- grid.locations$x+rx
      grid.locations$y.coord <- grid.locations$y+ry
      #find which x,y coords are within the region
      pts <- sp::SpatialPoints(data.frame(x = grid.locations$x.coord, y = grid.locations$y.coord))
      #Extract shape for current strata
      strata.sp <- as(region@region[[sf.column]][strat], "Spatial")
      inside <- pts[strata.sp,]
      pop.locations <- as.data.frame(inside@coords)
      #Record strata ID
      pop.locations$strata <- rep(strat, nrow(pop.locations))
      if(strat == 1){
        all.pop.locations <- oop.locations
      }else{
        all.pop.locations <- rbind(all.pop.locations, pop.locations)
      }
    }
  }
  return(all.pop.locations)
}


