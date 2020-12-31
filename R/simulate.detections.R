#' @importFrom stats rbinom
simulate.detections <- function(poss.distances, detectability){
  #calculate.probability of detection
  probs <- switch(detectability@key.function,
                  # Half-normal
                  "hn" = exp(-poss.distances$distance^2/(2*poss.distances$scale.param^2)),
                  # Hazard rate
                  "hr" = 1-exp(-(poss.distances$distance/poss.distances$scale.param)^-poss.distances$shape.param),
                  # Uniform
                  "uf" = poss.distances$scale.param
                  )
  detected <- rbinom(length(probs), 1, probs)
  dist.data <- poss.distances[detected == 1,]
  # Add object detection id's (note that 2 animals can be detected from
  # different transects and will have different object ids)
  if(nrow(dist.data) > 0){
    dist.data <- cbind(object = 1:nrow(dist.data), dist.data)
  }
  return(dist.data)
}
