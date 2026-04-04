#' Quantile function for a zero-truncated Negative Binomial distribution
#' 
#' @param p vector of probabilities
#' @param mean target mean
#' @param sd target standard deviation
#' @return vector of quantiles
qztruncnbinom <- function(p, mean = NA, sd = NA) {
  
  var <- sd^2
  
  if (mean < 3) stop("Target mean must be larger than 3.", call. = FALSE)
  if (var/mean < 1.2 || var/mean > 1.8) {
    stop("Variance/mean ratio must be in [1.2, 1.8].", call. = FALSE)
  }
  
  mean_ztnbinom <- function(N, p_param) N * p_param / (1 - (1 + p_param)^-N)
  
  var_ztnbinom <- function(N, p_param) {
    (N * p_param * (1 + p_param)) / (1 - (1 + p_param)^-N) * (1 - (N * p_param / (1 + p_param)) * (1 / (1 - (1 + p_param)^-N) - 1))
  }
  
  d_ztnbinom_book <- function(k, N, p) {
    generalized_choose <- function(n, k) {
      return(exp(lgamma(n + 1) - lgamma(k + 1) - lgamma(n - k + 1)))
    }
    prob = (1/(1-((1+p)^-N))) * generalized_choose(n = N+k-1, k = N-1) * (p/(1+p))^k * (1 - (p/(1+p)))^N
    return(prob)
  }
  
  error_ftn <- function(params) {
    (mean_ztnbinom(params[1], params[2]) - mean)^2 + 
      (var_ztnbinom(params[1], params[2]) - var)^2
  }
  
  start_guesses <- c(mean * 0.9, (2.096 - 2.362 * (mean / var)))
  est_params <- optim(par = start_guesses, fn = error_ftn, method = "L-BFGS-B",
                      lower = c(0.001, 0.00001), upper = c(Inf, 0.99999))$par
  
  # Core CDF-to-quantile mapping
  get_quantile <- function(target_prob) {
    if (target_prob < 0 || target_prob > 1) return(NaN)
    if (target_prob == 0) return(1)   # ZTNB domain is strictly positive integers
    if (target_prob == 1) return(Inf) # Upper tail is unbounded
    
    cumulative_prob <- 0
    k <- 1
    
    # Accumulate PMF iteratively until it crosses the target probability
    while (TRUE) {
      cumulative_prob <- cumulative_prob + d_ztnbinom_book(k, est_params[1], est_params[2])
      if (cumulative_prob >= target_prob) return(k)
      k <- k + 1
    }
  }
  
  # Map over the input probability vector
  vapply(p, get_quantile, numeric(1))
}
