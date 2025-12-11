#' Randomly generates values from a 
#' zero-truncated Negative Binomial distribution
#'
#' Generates values from a zero-truncated Negative Binomial distribution with 
#' mean and variance equal to that specified. 
#' It uses an optimization routine to check which values of parameters N and p 
#' will give values with the requested mean and variance.
#'
#' @param n number of values to randomly generate
#' @param mean mean of the generated values
#' @param var variance of the generated values
#' @return returns a randomly generated value from a zero-truncated 
#' Negative Binomial distribution.
#' @note Internal function not intended to be called by user.
#' @author Jack Nowacek
#' @importFrom stats runif optim
#' @importFrom dplyr tibble
#'
rztnbinom <- function(n.vals, mean = NA, var = NA){

  input_check <- function(mean, var) {
    
    # 1. Check for small mean (Mean < 3)
    if (mean < 3) {
      warning("Target mean (", mean, ") is less than 3. Optimization may be unstable or inaccurate in this region.")
    }
    
    # 2. Check for Variance/Mean ratio outside [1.2, 1.8]
    if (!is.na(mean) && !is.na(var) && mean > 0) {
      ratio <- var / mean
      if (ratio < 1.2 || ratio > 1.8) {
        warning(
          "Variance/Mean ratio (", round(ratio, 3), ") is outside the stable range of [1.2, 1.8]. ",
          "Optimization may be unstable or inaccurate in this region."
        )
      }
    }
  }
  input_check(mean, var)
  
  # calculates the mean of the ztnbinom function based on the input parameters, N and p
  mean_ztnbinom_book <- function(N, p) {
    mean = N*p / (1-((1+p)^-N))
    return(mean)
  }
  
  # calculates the variance of the ztnbinom function based on the input parameters, N and p
  var_ztnbinom_book <- function(N, p) {
    var = ( (N*p*(1+p)) / (1-((1+p)^-N)) ) * ( 1 - (N*p / (1+p)) * ( 1 / (1-((1+p)^-N)) - 1)  )
    return(var)
  }
  
  # defines a function that returns the probability of an input value, k, 
  # in the zero-truncated negative binomial function with the specified N and p
  
  # Generalized Choose function based on information in:
  # https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/Special
  
  d_ztnbinom_book <- function(k, N, p) {
    generalized_choose <- function(n, k) {
      return(exp(lgamma(n + 1) - lgamma(k + 1) - lgamma(n - k + 1)))
    }
    prob = (1/(1-((1+p)^-N))) * generalized_choose(n = N+k-1, k = N-1) * (p/(1+p))^k * (1 - (p/(1+p)))^N
    return(prob)
  }
  
  # defines a function that returns the cumulative probability up to an input 
  # value, k, in the zero-truncated negative binomial function with the specified N and p
  cdf_ztnbinom_book <- function(k, N, p) {
    cumulative_prob <- 0
    for (i in 1:k) {
      cumulative_prob = cumulative_prob + d_ztnbinom_book(i, N, p)
    }
    return(cumulative_prob)
  }
  
  # 'price is right' function
  # takes a value x, and a vector y, finds and returns the y value for which
  # x is closest to without going over
  pir <- function(x, y){
    val <- min(y[y >= x])
    return(val)
  }
  
  # Find parameters to best fit a desired mean and variance
  find_parameters <- function(goal_mean, goal_variance) {
    # minimize the difference between calculated and desired mean and variance
    error_ftn <- function(params) {
      N <- params[1]
      p <- params[2]
      book_mean <- mean_ztnbinom_book(N, p)
      book_var  <- var_ztnbinom_book(N, p)
      error <- (book_mean - goal_mean)^2 + (book_var - goal_variance)^2
      return(error)
    }
    
    start_guesses <- c(goal_mean*0.9, (2.096 - 2.362*(goal_mean/goal_variance)))
    result <- optim(
      par = start_guesses,
      fn = error_ftn,
      method = "L-BFGS-B",
      lower = c(0.001, 0.00001), 
      upper = c(Inf, 0.99999)
    )
    
    return(list(
      est_N = result$par[1],
      est_p = result$par[2]
    ))
  }
  est_params <- find_parameters(goal_mean = mean, goal_variance = var)
  
  # defines a function that returns n draws from a zero truncated 
  # negative binomial distribution with the input N and p as parameters. 
  # sets upper bound for how many cdf values to calculate, keeps sampler fast
  upper = est_params$est_N*10
  cdf_vals <- rep(0, upper)
  return_vals <- rep(0, upper)
  for (i in 1:upper) {
    return_vals[i] <- i
    cdf_vals[i] <- cdf_ztnbinom_book(k = i, N = est_params$est_N, p = est_params$est_p)
  }
  
  tab <- data.frame(return_vals, cdf_vals)
  
  # sets bounds for upper and lower limit of the unif() draw function 
  # min = cdf_ztnbinom_book(k = 1, p = N, p = N)
  min = 0
  max = cdf_ztnbinom_book(k = upper, N = est_params$est_N, p = est_params$est_p) # can do e^huge
  # max = 1
  draws <- rep(0, n.vals)
  for (i in 1:n.vals) {
    unif_draw <- runif(n = 1, min = min, max = max)
    # which value of the cdf is just higher than this value
    cdf_val_of_draw <- pir(x = unif_draw, y = cdf_vals)
    draw <- tab$return_vals[tab$cdf_vals == cdf_val_of_draw]
    draws[i] <- draw
  }
  return(draws)
}