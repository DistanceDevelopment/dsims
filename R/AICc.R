#' @importFrom stats AIC
AICc <- function(dfmodel){
  # Input - object returned by Distance::ds
  # Returns - AIC adjusted for small sample size
  n <- length(dfmodel$ddf$fitted)
  k <- length(dfmodel$ddf$par)
  AICc <- stats::AIC(dfmodel)$AIC + (2*k^2 + 2*k)/(n-k-1)
  return(AICc)
}
