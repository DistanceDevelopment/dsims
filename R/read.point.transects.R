read.point.transects <- function(filename, design, warnings = list(), rep = NA){
  # Function to read in shapefile and create a Point.Transect object

  # Information extracted from design
  # - design
  # - strata.area
  # - effort.allocation
  # - spacing
  # - design.angle
  # - edge.protocol

  # Information obtained from shapefile
  # - samplers
  # - cov.area
  # - cov.area.polys
  # - samp.count

  all.transects <- sf::read_sf(filename)

  # Get geometry
  sf.column <- attr(all.transects, "sf_column")
  sf.geom <- all.transects[[sf.column]]

  # Check that it is of type point
  if(!is(sf.geom, "sfc_POINT")){
    warnings <- message.handler(warnings, paste("The following file cannot be used, shapefile not of type point: ", filename, sep = ""), rep)
    return(list(transects = NULL, warnings = warnings))
  }

  # Process the shape and create a Point.Transect object
  transects <- process.point.transects(transects = all.transects,
                                       design = design,
                                       warnings = warnings,
                                       rep = rep)
  return(transects)
}

process.point.transects <- function(transects, design, warnings = list(), rep = NA){
  # Split into sub function for testing purposes

  # Process the shapefile in case it came from Distance for Windows
  transects <- process.dist.shapes(transects, design@region)

  # Find covered areas
  cov.areas <- get.covered.area.points(transects,
                                       design@truncation,
                                       design@region)

  # Extract information
  sampler.count <- cov.areas$sampler.count
  areas <- cov.areas$area
  cov.area.polys <- cov.areas$polys

  #Make a survey object
  transect <- new(Class="Point.Transect",
                  design = design@design,
                  points = transects,
                  samp.count = sampler.count,
                  effort.allocation = design@effort.allocation,
                  spacing = design@spacing,
                  design.angle = design@design.angle,
                  edge.protocol = design@edge.protocol,
                  cov.area = areas,
                  cov.area.polys = cov.area.polys ,
                  strata.area = design@region@area,
                  strata.names = design@region@strata.name)

  return(list(transects = transect, warnings = warnings))
}
