#' @importFrom sf st_buffer st_intersection st_area st_sfc st_sf
get.covered.area.points <- function(samplers, truncation, region, clip.to.strata = TRUE){
  # Input: shape - an sf points shape representing the transects
  #        truncation - the truncation distance
  #        region - and object of class Region representing the associated study area
  #        clip.to.strata - currently always true, in the future might relax to clip to study region.
  # Output: an sf polygon object with the same crs as shape representing the covered areas around 
  #        the transects.
  # Find the sf column
  sf.column <- attr(samplers, "sf_column")
  # Get sf col for region
  sf.reg.column <- attr(region@region, "sf_column")
  # Set up storage list
  polys <- list()
  # Set a flag
  first <- TRUE
  # Get strata names
  strata.names <- region@strata.name
  sampler.count <- cov.area <- rep(0, length(strata.names))
  # For each strata
  for(strat in seq(along = strata.names)){
    # Find the corresponding transects
    index <- which(samplers$strata == strata.names[strat])
    # If there are transects in the strata
    if(length(index) > 0){
      # Extract relevant transects
      strat.samplers <- samplers[index,][[sf.column]]
      # Generate buffers around each transect and store in list element
      cov.area.polys <- lapply(strat.samplers, FUN = sf::st_buffer, dist = truncation) 
      if(clip.to.strata){
        # Extract relevant polygon
        strata <- region@region[[sf.reg.column]][[strat]]
        # Clip to polygon boundary
        cov.area.polys <- lapply(cov.area.polys, sf::st_intersection, y = strata)
      } 
      #polys[[strat]] <- cov.area.polys
      if(first){
        temp.poly <- sf::st_sfc(cov.area.polys)
        transect.ids <- samplers$transect[index]
        strata.id <- rep(strata.names[strat], length(strat.samplers))
        sampler.count[strat] <- length(strat.samplers)
        cov.area[strat] <- sum(sf::st_area(sf::st_sfc(cov.area.polys)))
        first <- FALSE
      }else{
        temp.poly <- c(temp.poly, sf::st_sfc(cov.area.polys))
        transect.ids <- c(transect.ids, samplers$transect[index])
        strata.id <- c(strata.id, rep(strata.names[strat], length(strat.samplers)))
        sampler.count[strat] <- length(strat.samplers)
        cov.area[strat] <- sum(sf::st_area(sf::st_sfc(cov.area.polys)))
      }
    }
  }
  all.polys <- sf::st_sf(data.frame(transect = transect.ids, strata = strata.id, geom = temp.poly))
  # Add coordinate reference 
  #Set crs
  transect.crs <- sf::st_crs(samplers)
  sf::st_crs(all.polys) <- transect.crs
  return(list(polys = all.polys, sampler.count = sampler.count, area = cov.area))
}
