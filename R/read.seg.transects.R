read.seg.transects <- function(filename, design, warnings = list(), rep = NA){
  # Read in shapefile
  # Identify type of shapefile (point/linestring/multilinestring)
  # Check shapefile type matches with specified design
  
  # Information extracted from design (general to all)
  # - design
  # - strata.area   
  # - effort.allocation 
  # - spacing       
  # - design.angle 
  # - edge.protocol 
  # Information extracted from design (segment)
  # - seg.length
  # - seg.threshold
  
  # Information obtained from shapefile (general to all)
  # - samplers 
  # - cov.area      
  # - cov.area.polys 
  # - samp.count
  # Information obtained from shapefile (line / segment )
  # - line.length
  
  # Information that cannot be obtained
  # - trackline
  # - cyclictrackline
  
  all.transects <- sf::read_sf(filename)
  # Get geometry
  sf.column <- attr(all.transects, "sf_column")
  sf.geom <- all.transects[[sf.column]]
  
  # Check that it is of type point
  if(!is(sf.geom, "sfc_MULTILINESTRING") && !is(sf.geom, "sfc_LINESTRING")){
    warnings <- message.handler(warnings, paste("The following file cannot be used, shapefile not of type line / multi-line: ", filename, sep = ""), rep)
    return(list(transects = NULL, warnings = warnings))
  }
  
  # Process the shapefile in case it came from Distance for Windows
  all.transects <- process.dist.shapes(all.transects, design@region)
  
  # Find covered areas
  cov.areas <- get.covered.area.lines(all.transects, 
                                      design@truncation, 
                                      design@region)
  
  # Extract information
  sampler.count <- cov.areas$sampler.count
  areas <- cov.areas$area
  line.length <- cov.areas$line.length
  cov.area.polys <- cov.areas$polys
  
  #Make a survey object
  transect <- new(Class="Segment.Transect", 
                  design = design@design, 
                  lines = all.transects,
                  samp.count = sampler.count, 
                  line.length = line.length,
                  seg.length = design@seg.length,
                  effort.allocation = design@effort.allocation, 
                  spacing = design@spacing, 
                  design.angle = design@design.angle, 
                  edge.protocol = design@edge.protocol, 
                  cov.area = areas, 
                  cov.area.polys = cov.area.polys , 
                  strata.area = design@region@area,
                  strata.names = design@region@strata.name,
                  trackline = numeric(0),
                  cyclictrackline = numeric(0),
                  seg.threshold = design@seg.threshold) 
  
  return(list(transects = transect, warnings = warnings))
}