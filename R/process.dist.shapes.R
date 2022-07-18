process.dist.shapes <- function(shape, region) {
  # This function reformats shapefiles coming from Distance for Windows.
  # Will also check perform a check and throw an error if information
  # in strata cannot be found.
  if (all(c("transect", "strata") %in% names(shape))) {
    # All required information exists so return
    return(shape)
  }
  # Change LinkID to transect
  if ("LinkID" %in% names(shape)) {
    index <- which(names(shape) == "LinkID")
    names(shape)[index] <- "transect"
  }
  # Check strata exists or create if possible
  if (length(region@strata.name) == 1 &&
      !("Stratum" %in% names(shape))) {
    shape <- cbind(strata = rep(region@strata.name, nrow(shape)), shape)
  } else if ("Stratum" %in% names(shape)) {
    index <- which(names(shape) == "Stratum")
    names(shape)[index] <- "strata"
    shape$strata <- region@strata.name[shape$strata]
  } else{
    stop(
      "Unrecognised shapefile attribute format, check strata is defined as an attribute in the shapefile."
    )
  }
  #If transect IDs are not unique renumber transects so they are unique
  if (length(unique(shape$transect)) != length(shape$transect)) {
    shape$transect <- 1:length(shape$transect)
  }
  return(shape)
}