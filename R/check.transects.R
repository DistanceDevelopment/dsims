#' @importFrom sf st_length
check.transects <- function(transects){
  # Takes in an object of type Line.Transect checks for empty polygons
  # in covered area. If there are any removes them and corresponding
  # transect. Gives a warning if transect length is > 1e-06. Returns
  # updated transects object
  threshold <- 1
  units(threshold) <- "cm"
  samplers <- transects@samplers
  lengths <- sf::st_length(samplers)
  cov.areas <- transects@cov.area.polys
  sf.column.ca <- attr(cov.areas, "sf_column")
  to.remove <- numeric()
  for(i in seq(along = cov.areas$transect)){
    ca <- cov.areas[[sf.column.ca]][[i]]
    if(length(ca) == 0){
      to.remove <- c(to.remove, i)
      if(lengths[i] > threshold){
        warning("Removing transect with empty geometry but transect length is > 1 cm. Please report this warning to the package developer.", immediate. = TRUE, call. = FALSE)
      }
    }
  }
  if(length(to.remove) > 0){
    transects@cov.area.polys <- cov.areas[-to.remove,]
    transects@samplers <- samplers[-to.remove,]
  }
  return(transects)
}
