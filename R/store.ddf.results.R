store.ddf.results <- function(detection.results, ddf.results, i, N.in.covered, num.successful.models){
  Nhat <- ddf.results$Nhat
  n <- length(ddf.results$fitted)
  True.Pa <- n/N.in.covered
  Ave.Pa <- n/Nhat
  ESW <- Ave.Pa * ddf.results$meta.data$width
  f0 <- 1/ESW
  detection.results[1, "True.Pa", i] <- True.Pa
  detection.results[1, "Pa", i] <- Ave.Pa
  detection.results[1, "ESW", i] <- ESW
  detection.results[1, "f(0)", i] <- f0
  #Find which model was selected
  detection.results[1, "SelectedModel", i] <- ddf.results$model.index
  if(!is.null(ddf.results$delta.criteria)){
    detection.results[1, "DeltaCriteria", i] <- ddf.results$delta.criteria
  }
  detection.results[1, "SuccessfulModels", i] <- num.successful.models
  return(detection.results)
}

