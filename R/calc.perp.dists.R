#' @importFrom graphics points
#' @importFrom sf st_drop_geometry st_crs st_join st_intersects st_distance st_geometry st_coordinates
#' @importFrom methods is
calc.perp.dists <- function(population, transects, plot = FALSE){
  # Calculates the possible detection distances to the transects
  # Arguments:
  #   population - object of S4 Population Class
  #   transects - object of S4 Line.Transect Class
  # Returns:
  #   A data frame of possible detection distances

  samplers <- transects@samplers
  covered.areas <- transects@cov.area.polys
  pop <- population@population
  sf.pop <- sf::st_as_sf(pop, coords = c("x", "y"))
  sf::st_crs(sf.pop) <- sf::st_crs(covered.areas)
  # One indexed spatial join is significantly faster than N per-transect intersections
  covered.lookup <- covered.areas[, c("transect")]
  joined <- suppressWarnings(
    sf::st_join(sf.pop, covered.lookup, join = sf::st_intersects, left = FALSE)
  )

  if(nrow(joined) == 0){
    new.dataframe <- data.frame()
    return(new.dataframe)
  }

  transect.ids <- as.character(joined$transect)
  sampler.ids <- as.character(samplers$transect)
  sampler.index <- match(transect.ids, sampler.ids)
  sampler.geom <- sf::st_geometry(samplers)[sampler.index]
  distances <- as.numeric(sf::st_distance(sf::st_geometry(joined), sampler.geom, by_element = TRUE))

  coords <- as.data.frame(sf::st_coordinates(joined))
  names(coords) <- c("x", "y")
  new.dataframe <- cbind(coords, sf::st_drop_geometry(joined))
  new.dataframe$Sample.Label <- joined$transect
  new.dataframe$distance <- distances
  index <- order(new.dataframe$individual)
  new.dataframe <- new.dataframe[index,]
  return(new.dataframe)
}
