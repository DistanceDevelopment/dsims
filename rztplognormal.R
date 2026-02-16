#' Randomly generates values from a 
#' zero-truncated Poisson-lognormal distribution
#'
#' Finds parameters mu and sigma that match the user-specified mean and variance
#' for a zero-truncated Poisson-lognormal distribution, then generates random draws.
#'
#' @param n number of values to randomly generate
#' @param mean target mean of the generated values
#' @param var target variance of the generated values
#' @param verbose boolean to print optimization details
#' @return vector of randomly generated values
rztplognormal <- function(n, mean = NA, var = NA, verbose = FALSE) {
  
  # 1. Input Checks
  if (mean < 1) warning("Target mean is very low; zero-truncation may be unstable.")
  if (!is.na(mean) && !is.na(var) && var <= mean) {
    warning(paste("Target variance (", var, ") <= Mean (", mean, "). ",
                  "Poisson-lognormal is naturally overdispersed. Optimization may fail."))
  }
  
  # 2. Optimization Setup
  find_parameters <- function(goal_mean, goal_var) {
    
    obj_fun <- function(par) {
      mu <- par[1]
      sigma <- par[2]
      
      # Penalty for invalid sigma
      if (sigma <= 0.001) return(1e20)
      
      # A. Calculate p0 (Probability of zero)
      p0 <- tryCatch(
        integrate(function(x) exp(-exp(x)) * dnorm(x, mean = mu, sd = sigma), 
                  lower = mu - 10*sigma, upper = mu + 10*sigma)$value,
        error = function(e) NA
      )
      
      # If integration fails or p0 is invalid, return penalty
      if (is.na(p0) || p0 >= 0.9999 || p0 < 0) return(1e20)
      
      # B. Calculate Moments (with Overflow Protection)
      # We check the exponent size before calculating exp() to avoid Inf
      term1_arg <- mu + 0.5 * sigma^2
      term2_arg <- 2 * mu + 2 * sigma^2
      
      # exp(700) is approx the limit for double precision. If we exceed this, punish.
      if (term1_arg > 700 || term2_arg > 700) return(1e20)
      
      m1_untrunc <- exp(term1_arg)
      # E[Y^2] = E[Lambda] + E[Lambda^2]
      m2_untrunc <- m1_untrunc + exp(term2_arg)
      
      # C. Truncated Moments
      mean_zt <- m1_untrunc / (1 - p0)
      m2_zt   <- m2_untrunc / (1 - p0)
      var_zt  <- m2_zt - mean_zt^2
      
      # D. Final check for valid numbers
      if (!is.finite(mean_zt) || !is.finite(var_zt)) return(1e20)
      
      # Objective: Squared Error
      err <- (mean_zt - goal_mean)^2 + (var_zt - goal_var)^2
      
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
  
  # Execute Optimization
  params <- find_parameters(mean, var)
  
  # 3. Rejection Sampling
  draws <- numeric(n)
  count <- 0
  
  # Safety break for infinite loops
  max_iter <- 1000
  iter <- 0
  
  while (count < n && iter < max_iter) {
    iter <- iter + 1
    needed <- n - count
    batch_size <- ceiling(needed * 1.5) 
    
    # 1. Sample lambda
    lambdas <- rlnorm(batch_size, meanlog = params$mu, sdlog = params$sigma)
    # 2. Sample counts
    candidates <- rpois(batch_size, lambda = lambdas)
    # 3. Filter Zeros
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
