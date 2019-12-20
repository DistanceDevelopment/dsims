#' @importFrom stats runif
#' @importFrom splancs as.points
generate.pop.N <- function(population.description, region.obj){
#This function generates a Population based on a fixed population size
  N <- population.description@N
  density.obj <- population.description@density
  first = TRUE
  #Get sf column
  sf.column <- attr(region.obj@region, "sf_column")
  for(strat in seq(along = density.obj@density.surface)){
    if(N[strat] > 0){
      n.cells <- nrow(density.obj@density.surface[[strat]])
      probs <- density.obj@density.surface[[strat]][["density"]]/sum(density.obj@density.surface[[strat]][["density"]])
      #sample more animals than required as some will fall outside the survey region
      samp <- suppressWarnings(sample(x = 1:n.cells, size = 2*N[strat], replace = TRUE, prob = probs))
      grid.locations <- density.obj@density.surface[[strat]][samp,]
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
      if(nrow(inside@coords) < N[strat]){
        warning(paste("DSsim is unable to generate the requested population size for strata ", strat, ". We recommend you check the spacing of the density grid is appropriate, it may need reducing. Population size requested = ", N[strat], ", Population size generated = ", nrow(grid.locations),".", sep = ""), call. = FALSE)
      }else{
        pop.locations <- as.data.frame(inside@coords[1:N[strat],])
      }
      # Add strata ID
      pop.locations$strata <- rep(strat, nrow(pop.locations))
      # Accumulate all location
      if(first){
        all.pop.locations <- pop.locations
        first <- FALSE
      }else{
        all.pop.locations <- rbind(all.pop.locations, pop.locations)
      }
    }
  }
  return(all.pop.locations)
}
