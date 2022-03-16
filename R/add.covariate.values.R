#' @importFrom stats rnorm rlnorm
#' @importFrom methods is
add.covariate.values <- function(pop.data, covariates){
  # Find number of strata
  strata.ids <- unique(pop.data$Region.Label)
  # Divide population data up by strata
  list.data <- list()
  for(strat in seq(along = strata.ids)){
    list.data[[strata.ids[strat]]] <- pop.data[pop.data$Region.Label == strata.ids[strat],]
  }
  # Add covariates strata by strata
  cov.names <- names(covariates)
  for(covar in seq(along = covariates)){
    for(strat in seq(along = strata.ids)){
      # Get the covariate info
      current.cov <- covariates[[covar]][[strat]]
      n.vals <- nrow(list.data[[strat]])
      if(is(current.cov, "data.frame")){
        cov.values <- sample(current.cov$level, n.vals, replace = TRUE, prob = current.cov$prob)
        list.data[[strata.ids[strat]]][[cov.names[covar]]] <- cov.values
      }else if(is(current.cov, "list")){
        distribution <- current.cov[[1]]
        params <- current.cov[[2]]
        cov.values <- switch(distribution,
                             normal = rnorm(n.vals, mean = params$mean, sd = params$sd),
                             poisson = rpois(n.vals, lambda = params$lambda),
                             ztruncpois = rtpois(n.vals, mean = params$mean),
                             lognormal = rlnorm(n.vals, meanlog = params$meanlog, sdlog = params$sdlog))
        list.data[[strata.ids[strat]]][[cov.names[covar]]] <- cov.values
      }
    }
  }
  # Put data back together
  for(strat in seq(along = list.data)){
    if(strat == 1){
      new.pop.data <- list.data[[strata.ids[strat]]]
    }else{
      new.pop.data <- rbind(new.pop.data, list.data[[strata.ids[strat]]])
    }
  }
  #Check if -ve cluster sizes were generated
  if(!is.null(new.pop.data$size)){
    if(any(new.pop.data$size <= 0)){
      warning("Cluster size values <= 0 were generated.", immediate. = TRUE, call. = FALSE)
    }
  }
  return(new.pop.data)
}
