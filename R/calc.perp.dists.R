#' @importFrom graphics points
#' @importFrom sp Polygon Polygons SpatialPolygons
#' @importFrom sf as_Spatial
#' @importFrom rgeos gIntersects
calc.perp.dists <- function(population, transects, plot = FALSE){
  # Calculates the possible detection distances to the transects
  # Arguments:
  #   population - object of S4 Population Class
  #   transects - object of S4 Line.Transect Class
  # Returns:
  #   A data frame of possible detection distances

  subset.buff.func <- function(i, sp.pop, samplers, cov.areas){
    #returns the locations of the population within the truncation distance of transect i.
    sf.column.t <- attr(samplers, "sf_column")
    samp <- samplers[[sf.column.t]][[i]]
    sf.column.ca <- attr(cov.areas, "sf_column")
    ca <- cov.areas[[sf.column.ca]][[i]]
    sfc.ca <- sf::st_sfc(ca)
    sp.ca <- sf::as_Spatial(sfc.ca)
    #Get population within covered area
    available_ind <- which(rgeos::gIntersects(sp.pop, sp.ca, byid=TRUE))
    #get the points in the region
    sub.pop <- sp.pop[available_ind,]
    #Extract just the dataframe
    sub.pop <- sub.pop@data
    #Find start and end point [note may be a multilinestring]
    if("LINESTRING" %in% class(samp)){
      start.X <- samp[1]
      start.Y <- samp[3]
      end.X <- samp[2]
      end.Y <- samp[4]
    }else if("MULTILINESTRING" %in% class(samp)){
      index.final <- length(samp)
      start.X <- samp[[1]][1,1]
      start.Y <- samp[[1]][1,2]
      end.X <- samp[[index.final]][2,1]
      end.Y <- samp[[index.final]][2,2]
    }else{
      stop("sampler is not of type linestring or multilinestring", call. = TRUE)
    }
    #now calculate dists to transect
    #find the angle between the transect and the vector from the animal to the start of the transect
    transect.angle <- atan2(end.Y-start.Y, end.X-start.X)
    animal.angle <- atan2(sub.pop$y-start.Y, sub.pop$x-start.X)
    delta.angle <- abs(animal.angle-transect.angle)
    delta.angle <- (ifelse(delta.angle > pi, 2*pi - delta.angle, delta.angle))
    #calculate the distance from the transect start to the animal (the hypotenuse)
    hyp <- sqrt((sub.pop$y-start.Y)^2+(sub.pop$x-start.X)^2)
    #calculate the perpendicular distance (the opposite side of the RA triangle)
    perp.dists  <- hyp*sin(delta.angle)
    #Add perp distances
    if(nrow(sub.pop) > 0){
      #Make new dataset
      new.data <- cbind(sub.pop,
                        Sample.Label = rep(samplers$transect[i], nrow(sub.pop)),
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
  # make individuals points
  sp.pop <- sp::SpatialPointsDataFrame(sp::SpatialPoints(pop[,c("x","y")]), pop)
  #get all possible detection distances
  sf.column.t <- attr(samplers, "sf_column")
  all.poss.detects <- lapply(1:length(samplers[[sf.column.t]]),
                             FUN = subset.buff.func,
                             sp.pop = sp.pop,
                             samplers = samplers,
                             cov.areas = covered.areas)
  #Build up into a single data.frame
  sub.pop.size <- 0
  first <- TRUE
  new.dataframe <- NULL
  for(i in seq(along = all.poss.detects)){
    if(!is.null(all.poss.detects[[i]]) && nrow(all.poss.detects[[i]]) > 0){
      if(first){
        new.dataframe <- all.poss.detects[[i]]
        sub.pop.size <- sub.pop.size + nrow(all.poss.detects[[i]])
        first <- FALSE
      }else{
        new.dataframe <- rbind(new.dataframe, all.poss.detects[[i]])
        sub.pop.size <- sub.pop.size + nrow(all.poss.detects[[i]])
      }
    }
  }
  #In the case there are no data
  if(is.null(new.dataframe)){
    new.dataframe <- data.frame()
  }
  return(new.dataframe)
}
