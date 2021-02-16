#' @importFrom graphics points
#' @importFrom sf as_Spatial
#' @importFrom rgeos gIntersects
calc.rad.dists <- function(population, transects, plot = FALSE){
  # Calculates the possible detection distances to the transects
  # Arguments:
  #   population - object of S4 Population Class
  #   transects - object of S4 Point.Transect Class
  # Returns:
  #   A data frame of possible detection distances

  subset.calc.dist <- function(i, sp.pop, samplers, cov.areas){
    # Find individuals within covered area of transect i
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
    #Get sampler coords
    x.coord <- samp[1]
    y.coord <- samp[2]
    rad.dists <- sqrt((x.coord - sub.pop$x)^2 + (y.coord - sub.pop$y)^2)
    #Add rad distances
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
  # make individuals points
  sp.pop <- sp::SpatialPointsDataFrame(sp::SpatialPoints(pop[,c("x","y")]), pop)
  #get all possible detection distances
  sf.column.t <- attr(samplers, "sf_column")
  all.poss.detects <- lapply(1:length(samplers[[sf.column.t]]),
                             FUN = subset.calc.dist,
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

