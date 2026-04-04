#' Calculates quantile values from a zero-truncated Poisson distribution
#'
#' Calculates quantiles from a zero-truncated Poisson distribution with mean
#' equal to that specified. It uses an optimization routine to check which 
#' value of lambda will give values with the requested mean.
#'
#' @param p values for which to calculate quantile values
#' @param mean mean of the distribution of interest
#' @return returns quantile values from a zero-truncated Poisson distribution.
#' @note Internal function not intended to be called by user.
#' @author Len Thomas
#' @importFrom stats ppois qpois optimize

qztpois <- function(p, mean = NA){
  
  if(is.na(mean) | (mean <= 1)) {
    warning("NAs produced")
    return(rep(NaN, length(p)))
  } else {
    
    # Find lambda
    obj.func <- function(lambda, mean){
      Ex <- lambda / (1 - exp(-lambda))
      return((mean - Ex)^2)
    }
    lambda <- optimize(obj.func, lower = mean - 1, upper = mean, mean = mean)$minimum
    
    # Generate quantiles
    offset <- ppois(0, lambda)
    adjusted_p <- p * (1 - offset) + offset
    vals <- qpois(adjusted_p, lambda)
    return(vals)
  }  
}
