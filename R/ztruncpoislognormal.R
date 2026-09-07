#' Generates values from a zero-truncated Poisson Log-normal distribution with 
#' mean and variance equal to that specified. 
#' It uses an optimization routine to check which values of the parameters 
#' mu and sigma will parametrize a distribution whose mean and variance match
#' the specified values.
#'
#' @param n number of values to randomly generate
#' @param mean target mean of the generated values
#' @param sd target standard deviation of the generated values
#' @param verbose boolean to print optimization details
#' @return vector of randomly generated values
#' @note Internal function not intended to be called by user.
#' @author Jack Nowacek
#' @importFrom stats runif optim integrate dnorm
#' @importFrom dplyr tibble
#' 
#' 
ztruncpoislognormal <- function(n, mean = NA, sd = NA, verbose = FALSE) {
  
  var = sd^2
  
  if (mean < 1) {
    stop("Target mean must be larger than 1.", call. = FALSE)
  }

  if (var <= mean) {
    stop("Square of the target standard deviation must be larger than the mean.", call. = FALSE)
  }
  
  # optimization function
  find_parameters <- function(goal_mean, goal_var) {
    
    obj_fun <- function(par) {
      mu <- par[1]
      sigma <- par[2]
      
      # A. calculate p0 (probability of zero)
      p0 <- tryCatch(
        integrate(function(x) exp(-exp(x)) * dnorm(x, mean = mu, sd = sigma), 
                  lower = mu - 10*sigma, upper = mu + 10*sigma)$value,
        error = function(e) NA
      )
      
      # if integration fails or p0 is invalid, return penalty
      if (is.na(p0) || p0 >= 0.9999 || p0 < 0) return(1e20)
      
      # calculate moments of the distribution
      # We check the exponent size before calculating exp() to avoid Inf
      term1_arg <- mu + 0.5 * sigma^2
      term2_arg <- 2 * mu + 2 * sigma^2
      
      m1_untrunc <- exp(term1_arg)
      # E[Y^2] = E[Lambda] + E[Lambda^2]
      m2_untrunc <- m1_untrunc + exp(term2_arg)
      
      # find the truncated moments using the probability of 0
      mean_zt <- m1_untrunc / (1 - p0)
      m2_zt   <- m2_untrunc / (1 - p0)
      var_zt  <- m2_zt - mean_zt^2
      
      # sets error 
      # err <- (mean_zt - goal_mean)^2 + (var_zt - goal_var)^2
      # Variance scale dominates absolute error, sacrificing mean accuracy. 
      # Using relative error balances the optimization evenly.
      err <- ((mean_zt - goal_mean) / goal_mean)^2 + ((var_zt - goal_var) / goal_var)^2
      
      if (!is.finite(err)) return(1e20)
      
      return(err)
    }
    
    # Initial guesses
    # We use Nelder-Mead first as it is more robust to "rough" surfaces than L-BFGS-B
    # Then we refine with L-BFGS-B if needed, or just stick to robust bounds.
    start_par <- c(mu = log(goal_mean) - 0.1, sigma = 0.5)
    
    # Using L-BFGS-B with the new robust obj_fun
    res <- optim(
      par = start_par,
      fn = obj_fun,
      method = "L-BFGS-B",
      lower = c(-Inf, 0.001), 
      upper = c(Inf, Inf)
    )
    
    if (verbose) {
      cat(sprintf("Optimization:\n  Target Mean: %.2f, Target Var: %.2f\n  Found Mu: %.4f, Sigma: %.4f\n  Convergence: %d\n", 
                  goal_mean, goal_var, res$par[1], res$par[2], res$convergence))
    }
    
    return(list(mu = res$par[1], sigma = res$par[2]))
  }
  
  # execute optimization
  params <- find_parameters(mean, var)
  
  # sampling
  draws <- numeric(n)
  count <- 0
  
  # ensures the sampler will not be stuck
  max_iter <- 1000
  iter <- 0
  
  while (count < n && iter < max_iter) {
    iter <- iter + 1
    needed <- n - count
    batch_size <- ceiling(needed * 1.5) 
    
    # 1. sample lambda
    lambdas <- rlnorm(batch_size, meanlog = params$mu, sdlog = params$sigma)
    # 2. sample counts
    candidates <- rpois(batch_size, lambda = lambdas)
    # 3. filter zeros
    valid <- candidates[candidates > 0]
    
    take <- min(length(valid), needed)
    if (take > 0) {
      draws[(count + 1):(count + take)] <- valid[1:take]
      count <- count + take
    }
  }
  
  if (iter == max_iter) warning("Sampler hit max iterations. Check parameters.")
  
  return(draws)
}
