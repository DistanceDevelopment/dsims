#' Function to evaluate the quantiles of values p from a zero-truncated 
#' poisson log-normal distribution
#'
#' @param p values for which to find quantiles
#' @param mean target mean of the generated values
#' @param sd target standard deviation of the generated values
#' @param n_sim number of simulations to use for quantile calculation
#' @return vector of quantiles
#' @note Internal function not intended to be called by user.
#' @author Jack Nowacek
#' @importFrom stats rnorm rpois optim dnorm
#' @importFrom dplyr tibble
#' 
 
qztruncpoislognormal <- function(p, mean = NA, sd = NA, n_sim = 1e5) {
  var <- sd^2
  
  if (mean < 1) stop("Target mean must be > 1.", call. = FALSE)
  if (var <= mean) stop("Variance must be larger than the mean.", call. = FALSE)
  
  obj_fun <- function(par) {
    mu <- par[1]
    sigma <- par[2]
    
    p0 <- tryCatch(
      integrate(function(x) exp(-exp(x)) * dnorm(x, mean = mu, sd = sigma), 
                lower = mu - 10*sigma, upper = mu + 10*sigma)$value,
      error = function(e) NA
    )
    
    if (is.na(p0) || p0 >= 0.9999 || p0 < 0) return(1e20)
    
    m1_untrunc <- exp(mu + 0.5 * sigma^2)
    m2_untrunc <- m1_untrunc + exp(2 * mu + 2 * sigma^2)
    
    mean_zt <- m1_untrunc / (1 - p0)
    var_zt  <- (m2_untrunc / (1 - p0)) - mean_zt^2
    
    err <- (mean_zt - mean)^2 + (var_zt - var)^2
    if (!is.finite(err)) return(1e20)
    return(err)
  }
  
  res <- optim(
    par = c(log(mean) - 0.1, 0.5),
    fn = obj_fun,
    method = "L-BFGS-B",
    lower = c(-Inf, 0.001)
  )
  
  # Generate oversized batch to efficiently guarantee n_sim valid draws
  batch_size <- ceiling(n_sim * 1.5)
  lambdas <- rlnorm(batch_size, meanlog = res$par[1], sdlog = res$par[2])
  candidates <- rpois(batch_size, lambda = lambdas)
  
  draws <- candidates[candidates > 0]
  
  if (length(draws) < n_sim) {
    warning("High zero-inflation: empirical resolution reduced.", call. = FALSE)
  } else {
    draws <- draws[1:n_sim]
  }
  
  # type = 1 calculates the inverse of the empirical CDF, ensuring integer quantiles
  unname(quantile(draws, probs = p, type = 1, na.rm = TRUE))
}