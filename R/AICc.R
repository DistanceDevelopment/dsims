AICc <- function(dsmodel){
  # Input - object returned by Distance::ds
  # Returns - AIC adjusted for small sample size
  n <- length(ds.model$model$ddf$fitted)
  k <- length(ds.model$model$ddf$par)
  AICc <- AIC(ds.model$model)$AIC + (2*k^2 + 2*k)/(n-k-1)
  return(AICc)
}
