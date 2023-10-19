#' @importFrom graphics points
#' @importFrom sf st_intersection st_drop_geometry st_crs
#' @importFrom methods is
#' @importFrom purrr reduce
calc.perp.dists <- function(population, transects, plot = FALSE){
  # Calculates the possible detection distances to the transects
  # Arguments:
  #   population - object of S4 Population Class
  #   transects - object of S4 Line.Transect Class
  # Returns:
  #   A data frame of possible detection distances

  subset.buff.func <- function(i, sf.pop, samplers, cov.areas){
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
    sub.pop.coords <- cbind(sub.pop.coords, st_drop_geometry(pop.in.cov))
    #Find start and end point [note may be a multilinestring]
    if(is(samp, "LINESTRING")){
      start.X <- samp[1]
      start.Y <- samp[3]
      end.X <- samp[2]
      end.Y <- samp[4]
    }else if(is(samp, "MULTILINESTRING")){
      index.final <- length(samp)
      start.X <- samp[[1]][1,1]
      start.Y <- samp[[1]][1,2]
      end.X <- samp[[index.final]][2,1]
      end.Y <- samp[[index.final]][2,2]
    }else{
      stop("sampler is not of type linestring or multilinestring", call. = TRUE)
    }
    #now calculate distances to transect
    #find the angle between the transect and the vector from the animal to the start of the transect
    transect.angle <- atan2(end.Y-start.Y, end.X-start.X)
    animal.angle <- atan2(sub.pop.coords$y-start.Y, sub.pop.coords$x-start.X)
    delta.angle <- abs(animal.angle-transect.angle)
    delta.angle <- (ifelse(delta.angle > pi, 2*pi - delta.angle, delta.angle))
    #calculate the distance from the transect start to the animal (the hypotenuse)
    hyp <- sqrt((sub.pop.coords$y-start.Y)^2+(sub.pop.coords$x-start.X)^2)
    #calculate the perpendicular distance (the opposite side of the RA triangle)
    perp.dists  <- hyp*sin(delta.angle)
    #Add perp distances
    if(nrow(sub.pop.coords) > 0){
      #Make new dataset
      new.data <- cbind(sub.pop.coords,
                        Sample.Label = rep(samplers$transect[i], nrow(sub.pop.coords)),
                        distance = perp.dists)
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
  sf::st_crs(sf.pop) <- sf::st_crs(covered.areas)
  #get all possible detection distances
  all.poss.detects <- lapply(1:nrow(samplers),
                             FUN = subset.buff.func,
                             sf.pop = sf.pop,
                             samplers = samplers,
                             cov.areas = covered.areas)
  
  #Build up into a single data.frame
  new.dataframe <- reduce(all.poss.detects, rbind)
  if(!is.null(new.dataframe)){
    # Order the data by individual
    index <- order(new.dataframe$individual)
    new.dataframe <- new.dataframe[index,]
  }else{
    new.dataframe <- data.frame()
  }
  # remove duplicate / redundant cols
  #index <- which(names(tmp4) %in% c("transect", "strata"))
  #ordered.data <- ordered.data[,-index]
  return(new.dataframe)
}
