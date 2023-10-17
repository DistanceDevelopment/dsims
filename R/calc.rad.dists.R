#' @importFrom graphics points
#' @importFrom sf st_intersection
#' @importFrom purrr reduce
calc.rad.dists <- function(population, transects, plot = FALSE){
  # Calculates the possible detection distances to the transects
  # Arguments:
  #   population - object of S4 Population Class
  #   transects - object of S4 Point.Transect Class
  # Returns:
  #   A data frame of possible detection distances

  subset.calc.dist <- function(i, sf.pop, samplers, cov.areas){
    # Find individuals within covered area of transect i
    #returns the locations of the population within the truncation distance of transect i.
    # Extract relevant sampler
    sf.column.t <- attr(samplers, "sf_column")
    samp <- samplers[[sf.column.t]][[i]]
    #Extract associated covered area
    cov.area <- cov.areas[cov.areas$transect == i,]
    # Find the population in the covered area of transect i
    pop.in.cov <- suppressWarnings(
      st_intersection(sf.pop, cov.area))
    #Turn into a data.frame
    sub.pop.coords <- as.data.frame(sf::st_coordinates(pop.in.cov))
    names(sub.pop.coords) <- c("x","y")
    # Add other info back in
    sub.pop <- cbind(sub.pop.coords, st_drop_geometry(pop.in.cov))
    # Get sampler coords
    x.coord <- samp[1]
    y.coord <- samp[2]
    rad.dists <- sqrt((x.coord - sub.pop$x)^2 + (y.coord - sub.pop$y)^2)
    # Add rad distances
    if(nrow(sub.pop) > 0){
      #Make new dataset
      new.data <- cbind(sub.pop,
                        Sample.Label = rep(samplers$transect[i], nrow(sub.pop)),
                        distance = rad.dists)
    }else{
      new.data <- NULL
    }
    return(new.data)
  }
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Main function begins
  samplers <- transects@samplers
  covered.areas <- transects@cov.area.polys
  pop <- population@population
  sf.pop <- st_as_sf(pop, coords = c("x", "y")) 
  #get all possible detection distances
  all.poss.detects <- lapply(1:nrow(samplers),
                             FUN = subset.calc.dist,
                             sf.pop = sf.pop,
                             samplers = samplers,
                             cov.areas = covered.areas)
  
  #Build up into a single data.frame
  new.dataframe <- 
    all.poss.detects %>% reduce(rbind)
  index <- order(new.dataframe$individual)
  ordered.data <- new.dataframe[index,]
  # remove duplicate / redundant cols
  #index <- which(names(tmp4) %in% c("transect", "strata"))
  #ordered.data <- ordered.data[,-index]
  return(ordered.data)
}

