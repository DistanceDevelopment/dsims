calculate.scale.param <- function(pop.data, detectability, region){
# This function calculates the scale parameters including any covariate effects
# and adds these values to the population dataframe which is then returned. Also
# adds the shape parameter where appropriate.
# ARGUMENTS:
#   pop.data - population dataframe
#   detectability - a detectablity object
#   region - a region object
# RETURN:
#   modified population dataframe
  
  # Get number of strata
  strata.no <- ifelse(length(region@strata.name) == 0, 1, length(region@strata.name))
  if(strata.no == 1){
     strata.names <- region@region.name
  }else strata.names <- region@strata.name
  # Check that the number of scale / shape params is 1 or equal to number of strata
  scale <- detectability@scale.param
  if(length(scale) == 1){
    detectability@scale.param <- rep(scale, strata.no)
  }else if(length(scale) != strata.no){
    warning("The number of scale parameters provided does not match the number of strata. Only the first value will be used.", call. = FALSE, immediate. = TRUE)
    detectability@scale.param <- rep(scale[1], strata.no)
  }
  if(detectability@key.function == "hr"){
    shape <- detectability@shape.param
    if(length(shape) == 1){
      detectability@shape.param <- rep(shape, strata.no)
    }else if(length(shape) > 0 & length(shape) != strata.no){
      warning("The number of shape parameters provided does not match the number of strata. Only the first value will be used.", call. = FALSE, immediate. = TRUE)
      detectability@shape.param <- rep(shape[1], strata.no)
    }
  }
  # Check the covariate names in detectability are present in the population
  pop.cov.names <- names(pop.data)
  detect.cov.names <- names(detectability@cov.param)
  if(!all(detect.cov.names %in% pop.cov.names)){
    warning("Not all covariates provided in the detecability description were
            provided for the population description. These additional covariates
            will be ignored.", call. = FALSE, immediate. = TRUE)
  }
  # Check if the number of parameters is the
  for(cov in seq(along = detectability@cov.param)){
    current.cov <- detectability@cov.param[[cov]]
    if(class(current.cov) != "data.frame"){
      if(length(current.cov == 1)){
        # repeat it for the number of strata
        detectability@cov.param[[cov]] <- rep(current.cov, strata.no)
      }else if(length(current.cov) != strata.no){
        warning(paste("The number of detection parameters for covariate ", detect.cov.names[cov]," does not match the number of strata. Only the first value will be used.", sep = ""), call. = FALSE, immediate. = TRUE)
        # repeat the first value for the number of strata
        detectability@cov.param[[cov]] <- rep(current.cov[1], strata.no)
      }
    }
  }
  # Check to see if any covariates are factor covariates.
  cov.factor <- logical(0)
  for(cov in seq(along = detectability@cov.param)){
    cov.factor[cov] <- is.data.frame(detectability@cov.param[[cov]])
  }
  # Divide data up by strata
  # Find number of strata
  strata.ids <- unique(pop.data$strata)
  # Divide population data up by strata
  list.data <- list()
  for(strat in seq(along = strata.ids)){
    strata.data <- pop.data[pop.data$strata == strata.ids[strat],] 
    temp.data <- strata.data
    # Find if any are factor level covariates
    if(any(cov.factor)){
      factor.cols <- character(0)
      index <- which(cov.factor)
      for(fac in seq(along = index)){
        # Need to change factors to dummy variables
        factor.tab <- detectability@cov.param[[index[fac]]]
        if(!is.null(factor.tab$strata)){
          # Subset table for strata
          factor.tab <- factor.tab[factor.tab$strata == strata.names[strata.ids[strat]],]
        }
        
        for(level in seq(along = factor.tab$level)){
          cov.name <- detect.cov.names[index[fac]]
          factor.level <- paste(cov.name, ".", factor.tab$level[level], sep = "")
          factor.cols <- c(factor.cols, factor.level)
          temp.data[[factor.level]] <- ifelse(temp.data[[cov.name]] == factor.tab$level[level], factor.tab$param[level], 0)
        }
      }
      # Sum factor columns
      sum.of.factors <- apply(temp.data[, factor.cols], 1, sum)
    }else{
      sum.of.factors <- rep(0, nrow(strata.data))
    }
    # Now deal with non factor covariates
    if(any(!cov.factor)){
      index <- which(!cov.factor)  
      cov.names <- detect.cov.names[index]
      temp.data <- strata.data[,cov.names]
      # Get param vector
      params <- numeric(0)
      for(cov in seq(along = cov.names)){
        params <- c(params, detectability@cov.param[[cov.names[cov]]][strat])
      }
      # Turn into matrix
      params <- as.matrix(params, nrow = 1)
      # Matrix multiplication
      temp.data <- as.matrix(temp.data)
      temp.multiply <- temp.data %*% params
    }else{
      temp.multiply <- rep(0, nrow(strata.data))
    }
    # Add it all together (including B0)
    log.scale <- log(detectability@scale.param[strat])
    sum.params <- sum.of.factors + temp.multiply
    new.scale <- exp(log.scale + sum.params)
    strata.data$scale.param <- as.numeric(new.scale)
    # If it is a hr function add shape param too
    if(detectability@key.function == "hr"){
      strata.data$shape.param <- rep(detectability@shape.param[strat], nrow(strata.data))
    }
    list.data[[strata.ids[strat]]] <- as.data.frame(strata.data)
  }
  # Merge the datasets again
  for(strat in seq(along = list.data)){
    if(strat == 1){
      new.pop.data <- list.data[[strata.ids[strat]]]
    }else{
      new.pop.data <- rbind(new.pop.data, list.data[[strata.ids[strat]]])
    }
  }
  return(new.pop.data)
}