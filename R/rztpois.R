#' Randomly generates values from a zero-truncated Poisson distribution
#'
#' Generates values from a zero-truncated Poisson distribution with mean
#' equal to that specified. It uses an optimisation routine to check which 
#' value of lambda will give values with the requested mean.
#'
#' @param n number of values to randomly generate
#' @param mean mean of the generated values
#' @return returns a randomly generated value from a zero-truncated Poisson
#' distribution.
#' @note Internal function not intended to be called by user.
#' @author Len Thomas
#' @importFrom stats runif dpois qpois optimize
#'
rztpois <- function(n, mean = NA){
  
  if(is.na(mean) | (mean <= 1)) {
    warning("NAs produced")
    return(rep(NaN, n))
  } else {
    
    # Find lambda
    obj.func <- function(lambda, mean){
      Ex <- lambda / (1 - exp(-lambda))
      return((mean - Ex)^2)
    }
    lambda <- optimize(obj.func, lower = mean - 1, upper = mean, mean = mean)$minimum
    
    # Generate deviates
    pmin <- dpois(0, lambda)
    u <- runif(n, min = pmin, max = 1)
    vals <- qpois(u, lambda)
    return(vals)
  }  
}