#' @importFrom stats runif
#' @importFrom sf st_crs st_coordinates st_as_sf st_intersection
generate.pop.N <- function(population.description, region){
#This function generates a Population based on a fixed population size
  N <- population.description@N
  density.obj <- population.description@density
  sf.density <- density.obj@density.surface[[1]]
  first = TRUE
  #Extract region and remove crs as we will work with spatial shapes
  temp.region <- region@region
  sf::st_crs(temp.region) <- NA
  #Set up storage
  all.pop.locations <- data.frame()
  #Iterate over strata
  for(strat in seq(along = region@strata.name)){
    # Extract strata data
    strata.data <- data.frame(x = sf.density$x[sf.density$strata == region@strata.name[strat]],
                             y = sf.density$y[sf.density$strata == region@strata.name[strat]],
                             density = sf.density$density[sf.density$strata == region@strata.name[strat]])
    if(N[strat] > 0){
      #Set a counter so cannot get stuck in an infinite loop!
      counter <- 1
      pop.locations <- data.frame()
      while(nrow(pop.locations) < N[strat] && counter < 10){
        #Generate some animal locations
        inside <- get.animal.locations(strata.data, 
                                       temp.region, 
                                       N[strat], 
                                       strat, 
                                       density.obj@x.space, 
                                       density.obj@y.space)
        inside <- as.data.frame(sf::st_coordinates(inside))
        # if no animals were in the region skip this loop
        if(nrow(inside) == 0){
          counter <- counter + 1
          next
        }
        names(inside) <- c("x","y")
        if(nrow(pop.locations) == 0){
          pop.locations <- inside[1:min(N[strat],nrow(inside)),]
        }else{
          #Number of animal locations still required
          n.more <- N[strat]-nrow(pop.locations)
          pop.locations <- rbind(pop.locations, inside[1:min(n.more, nrow(inside)),])
        }
        #Increment counter so we leave loop after 10 iterations even if we still don't have enough animals
        counter <- counter + 1
      }
      if(nrow(pop.locations) < N[strat]){
        warning(paste("DSsim is unable to generate the requested population size for strata ", strat, ". We recommend you check the spacing of the density grid is appropriate, it may need reducing. Population size requested = ", N[strat], ", Population size generated = ", nrow(pop.locations),".", sep = ""), call. = FALSE)
      }
      # Add strata ID
      pop.locations$Region.Label <- rep(region@strata.name[strat], nrow(pop.locations))
      # Accumulate all location
      if(nrow(all.pop.locations) == 0){
        all.pop.locations <- pop.locations
      }else{
        all.pop.locations <- rbind(all.pop.locations, pop.locations)
      }
    }
  }
  return(all.pop.locations)
}

#' @importFrom sf as_Spatial
get.animal.locations <- function(strata.data, temp.region, Nj, strat, x.space, y.space){
  # Number of grid cells
  n.cells <- nrow(strata.data)
  # Sampling probabiltites
  probs <- strata.data$density/sum(strata.data$density)
  #sample more animals than required as some will fall outside the survey region
  samp <- suppressWarnings(sample(x = 1:n.cells, size = 2*Nj, replace = TRUE, prob = probs))
  grid.locations <- strata.data[samp,]
  #generate random locations within grid cell
  rx <- runif(nrow(grid.locations), -x.space/2, x.space/2)
  ry <- runif(nrow(grid.locations), -y.space/2, y.space/2)
  # Put x,y coords of animals in a data frame
  pts <- data.frame(x = grid.locations$x+rx, y = grid.locations$y+ry)
  # Make it an sf object
  pts.sf <- sf::st_as_sf(pts, coords = c("x", "y"))
  inside <- suppressWarnings(sf::st_intersection(pts.sf, temp.region[strat,]))
  return(inside)
}

