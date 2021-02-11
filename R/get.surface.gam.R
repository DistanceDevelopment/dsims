#' @importFrom sp Polygon Polygons SpatialPolygons
#' @importFrom sf as_Spatial
get.surface.gam <- function(region, x.space, y.space, gam.model, buffer){

  # Check the value of the buffer
  if(length(buffer) == 0){
    buffer <- min(x.space, y.space)
  }
  #Creates a density surface with a constant value across the whole survey region
  #Create a rectangular grid over the entire region
  temp.region <- region@region
  #Need to remove crs as going to work with sp objects
  sf::st_crs(temp.region) <- NA
  #Get bounding box and create a grid of points across whole area
  bbox <- sf::st_bbox(temp.region)
  region.width <- bbox[["xmax"]]-bbox[["xmin"]]
  region.height <- bbox[["ymax"]]-bbox[["ymin"]]
  no.x.ints <- ceiling((region.width+2*buffer)/x.space)
  no.y.ints <- ceiling((region.height+2*buffer)/y.space)
  x.adj <- (x.space*no.x.ints - region.width)/2
  y.adj <- (y.space*no.y.ints - region.height)/2
  x.vals <- seq(bbox[["xmin"]]-x.adj, bbox[["xmax"]]+x.adj, by = x.space)
  y.vals <- seq(bbox[["ymin"]]-y.adj, bbox[["ymax"]]+y.adj, by = y.space)
  temp.coords <- expand.grid(x.vals, y.vals)
  names(temp.coords) <- c("x","y")
  pts <- sp::SpatialPoints(temp.coords)

  # Create buffered regions rather than jittering the grid points
  density.surfaces <- list()
  sf.column <- attr(temp.region, "sf_column")
  for(strat in seq(along = temp.region[[sf.column]])){
    #Extract polygons and gaps for current strata
    strata.sp <- sf::as_Spatial(temp.region[[sf.column]][strat])
    # Add positive buffer region
    buffered.strata <- rgeos::gBuffer(strata.sp, width = x.space)
    inside <- pts[buffered.strata,]
    gridpoints <- as.data.frame(inside@coords)
    predicted.values <- mgcv::predict.gam(gam.model, newdata = gridpoints, type = "response")
    gridpoints$density <- predicted.values
    density.surfaces[[strat]] <- gridpoints
  }
  return(density.surfaces)
}

