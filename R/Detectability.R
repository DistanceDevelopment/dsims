#' @include Population.Description.R

#' Class "Detectability"
#'
#' Class \code{"Detectability"} is an S4 class describing the probability
#' of detecting individuals / clusters in a population.
#'
#' @name Detectability-class
#' @title S4 Class "Detectability"
#' @docType class
#' @slot key.function Object of class \code{"character"}; a code
#' specifying the detection function form ("hn" = half normal, "hr" =
#' hazard rate.)
#' @slot scale.param Object of class \code{"numeric"}; The scale
#' parameter for the detection function.
#' @slot shape.param Object of class \code{"numeric"}; The shape
#' parameter for the detection function.
#' @slot cov.param Object of class \code{"numeric"}; The parameter
#' values associated with the covariates. Not yet implemented
#' @slot truncation Object of class \code{"numeric"}; The maximum
#' distance at which objects may be detected.
#' @keywords classes
#' @export
#' @seealso \code{\link{make.detectability}}
setClass("Detectability", representation(key.function    = "character",
                                         scale.param     = "numeric",
                                         shape.param     = "numeric",
                                         cov.param       = "list",
                                         truncation      = "numeric"))
#' @importFrom methods validObject
setMethod(
  f="initialize",
  signature="Detectability",
  definition=function(.Object, key.function = "hn", scale.param = 25, shape.param = numeric(0), covariates = character(0), cov.param = list(), truncation = 50){
    #Input pre-processing
    # - this needs to be done here as cannot alter object inside validation method
    cov.names <- names(cov.param)
    for(cov in seq(along = cov.param)){
      if(class(cov.param[[cov]]) == "data.frame"){
        if(!all(names(cov.param[[cov]]) %in% c("strata", "level", "param"))){
          index <- which(!(names(cov.param[[cov]]) %in% c("strata", "level", "param")))
          warning(paste("The dataframe for covariate '", cov.names[cov], "' has unrecognised columns: ", names(cov.param[[cov]])[index],". These will be ignored.", sep = ""), call. = FALSE, immediate. = TRUE)
          df.new <- cov.param[[cov]]
          if(ncol(df.new) > 2){ # if there are 2 or less then we will not be here!
            # removing 1 col from 2 col df results in non-named vector!
            index <- which(names(cov.param[[cov]]) %in% c("strata", "level", "param"))
            cov.param[[cov]] <- df.new[,index]
          }
        }
      }
    }

    #Set slots
    .Object@key.function <- key.function
    .Object@scale.param  <- scale.param
    .Object@shape.param  <- shape.param
    .Object@cov.param    <- cov.param
    .Object@truncation   <- truncation
    #Check object is valid
    valid <- validObject(.Object, test = TRUE)
    if(class(valid) == "character"){
      stop(paste(valid), call. = FALSE)
    }
    # return object
    return(.Object)
  }
)

setValidity("Detectability",
            function(object){
              if(length(object@key.function) > 1){
                warning("You must only supply one key function, only the first will be used.", call. = FALSE, immediate. = TRUE)
                object@key.function <- object@key.function[1]
              }
              if(!object@key.function%in%c("hr","hn","uf")){
                return("Unsupported key function, please select hn, hr or uf.")
              }
              if(object@key.function == "hr" && length(object@shape.param) == 0){
                return("You have selected the hazard rate model but not supplied a shape parameter.")
              }
              if(object@key.function != "hr" && length(object@shape.param) > 0){
                warning(paste("You have selected the ", object@key.function, " key function and supplied a shape parameter value, this will be ignored.", sep = ""), call. = FALSE, immediate. = TRUE)
                object@shape.param <- numeric(0)
              }
              for(i in seq(along = object@scale.param)){
                if(object@scale.param[i] <= 0){
                  return("Invalid scale parameter. The scale parameter must be greater than zero.")
                }else if(object@key.function == "uf" && object@scale.param[i] > 1){
                  return("Invalid scale parameter. The scale parameter must be greater than zero and less than or equal to 1 for the uniform distribution.")
                }
              }
              for(i in seq(along = object@shape.param)){
                if(object@shape.param[i] < 0){
                  return("Invalid shape parameter. Must be greater than or equal to zero.")
                }
              }
              if(length(object@scale.param) > 0 && length(object@shape.param) > 0 && length(object@scale.param) != length(object@shape.param)){
                return("The same number of values must be provided for both the shape and scale parameters or only one value supplied for either the shape or scale parameter.")
              }
              cov.names <- names(object@cov.param)
              if(any(cov.names == "")){
                return("Not all the elements of the cov.param list are named. Please provide names for all elements.")
              }
              for(cov in seq(along = object@cov.param)){
                if(length(object@cov.param[[cov]]) == 0){
                  return(paste("List element ", cov.names[cov], " of the cov.param list does not contain any values.", sep = ""))
                }else if(class(object@cov.param[[cov]]) == "data.frame"){
                  if(!all(c("level", "param") %in% names(object@cov.param[[cov]]))){
                    index <- which(!(c("level", "param") %in% names(object@cov.param[[cov]])))
                    return(paste("The dataframe for covariate '", cov.names[cov], "' has missing columns: ", c("level", "param")[index], sep = ""))
                  }
                }
              }
              return(TRUE)
            }
)

# GENERIC METHODS DEFINITIONS --------------------------------------------

#' @rdname plot.Detectability-methods
#' @return No return value, gives a warning to the user
#' @exportMethod plot
setMethod(
  f="plot",
  signature="Detectability",
  definition=function(x, y, add = FALSE, plot.units = character(0), region.col = NULL, gap.col = NULL, main = "", ...){
    stop("Please provide a y argument of class Population.Description as well as detectability to enable plotting.", call. = FALSE)
  })

#' Plot
#'
#' Plots an S4 object of class 'Detectability'
#'
#' @param x object of class Detectability
#' @param y object of class Population.Description
#' @param add logical indicating whether it should be added to
#'  existing plot
#' @param plot.units allows for units to be converted between m
#'  and km
#' @param region.col fill colour for the region
#' @param gap.col fill colour for the gaps
#' @param main character plot title
#' @param ... other general plot parameters
#' @return No return value, plotting function
#' @rdname plot.Detectability-methods
#' @importFrom graphics polygon plot axTicks axis lines plot legend par
#' @importFrom stats quantile
#' @exportMethod plot
setMethod(
  f="plot",
  signature=c("Detectability","Population.Description"),
  definition=function(x, y, add = FALSE, plot.units = character(0), region.col = NULL, gap.col = NULL, main = "", ...){
    object <- x
    pop.desc <- y
    # Find out how many covariates there are
    no.covariates <- length(object@cov.param)
    cov.names <- names(object@cov.param)
    # Check if there are covariates in detectability that are not in pop.desc
    pop.covs <- names(pop.desc@covariates)
    if(any(!cov.names %in% pop.covs)){
      stop("You have defined detectability for covariates that are not included in the population description.", call. = FALSE)
    }
    # set mfrow storing old settings
    mfrow.value <- switch(as.character(no.covariates),
                          "0" = c(1,1),
                          "1" = c(1,1),
                          "2" = c(1,2),
                          "3" = c(2,2),
                          "4" = c(2,2),
                          "5" = c(2,3),
                          "6" = c(2,3),
                          "7" = c(3,3),
                          "8" = c(3,3),
                          "9" = c(3,3))
    # set on exit to return to old settings
    oldparams <- par(mfrow = mfrow.value)
    on.exit(par(oldparams))
    # generate x values to truncation
    x <- seq(0, object@truncation, length = 200)
    # get the scale and shape parameters
    scale.param <- object@scale.param
    shape.param <- object@shape.param
    if(length(object@cov.param) > 0){
      # iterate through all the covariates and make plots
      for(cov in seq(along = object@cov.param)){
        cov.params <- object@cov.param[[cov]]
        cov.dist <- pop.desc@covariates[[cov.names[cov]]]
        if(class(object@cov.param[[cov]]) == "data.frame"){
          param.type = "categorical"
          no.cov.strata <- ifelse(is.null(cov.params$strata), 1, length(unique(cov.params$strata)))
        }else if(class(cov.dist[[1]]) == "data.frame"){
          param.type = "discrete"
          no.cov.strata <- length(cov.params)
        }else{
          param.type = "continuous"
          no.cov.strata <- length(cov.params)
        }
        if(param.type == "categorical"){
          plot.title <- paste("Covariate: ", cov.names[cov], " (factor)", sep = "")
        }else if(param.type == "discrete"){
          plot.title <- paste("Covariate: ", cov.names[cov], " (discrete)", sep = "")
        }else{
          plot.title <- paste("Covariate: ", cov.names[cov], " (continuous)", sep = "")
        }
        # set up initial plot
        plot(0,0, xlim = c(0,object@truncation + object@truncation*0.05), ylim = c(0,1.2), main = plot.title, col = "white", xlab = "Distance", ylab = "Detection Probability")
        no.strata <- max(length(scale.param), length(shape.param), no.cov.strata)
        # make up storage array
        if(param.type == "categorical"){
          nlevels <- length(unique(cov.params$level))
        }else{
          nlevels <- 3
        }
        y <- array(NA, dim = c(nlevels, 200, no.strata))
        # iterate through strata
        for(strat in 1:no.strata){
          # basic scale param
          if(length(scale.param) == no.strata){
            scale.param.strat <- scale.param[strat]
          }else{
            scale.param.strat <- scale.param[1]
          }
          # if hazard rate do the same for shape parameter
          if(object@key.function == "hr"){
            if(length(shape.param) == no.strata){
              shape.param.strat <- shape.param[strat]
            }else{
              shape.param.strat <- shape.param[1]
            }
          }
          if(param.type == "categorical"){
            # get scale adjustment for each level
            if(!is.null(cov.params$strata)){
              scale.adjustments <- cov.params$param[cov.params$strata == pop.desc@strata.names[strat]]
            }else{
              scale.adjustments <- cov.params$param
            }
            # new scale params
            new.scale.params <- exp(log(scale.param.strat) + scale.adjustments)
            # generate y vals
            for(i in seq(along = new.scale.params)){
              y[i,,strat] <- switch(object@key.function,
                                    "hn" = exp(-x^2/(2*new.scale.params[i]^2)),
                                    "hr" = 1-exp(-(x/new.scale.params[i])^-shape.param.strat),
                                    "uf" = rep(new.scale.params[i], length(x)))
            }
          }else{
            # if the covariate distribution is discrete but not categorical
            if(param.type == "discrete"){
              if(length(cov.dist) == no.strata){
                temp.dist <- cov.dist[[strat]]
              }else{
                temp.dist <- cov.dist[[1]]
              }
              dist.mean <- sum(temp.dist$level*temp.dist$prob)
              dist.min <- min(temp.dist$level)
              dist.max <- max(temp.dist$level)
              quantiles <- c(dist.min, dist.mean, dist.max)
            }else if(param.type == "continuous"){
              # get 2.5% and 97.5% quantiles for covariate values
              if(length(cov.dist) == no.strata){
                cov.strat.dist <- cov.dist[[strat]]
              }else{
                cov.strat.dist <- cov.dist[[1]]
              }
              dist.param <- cov.strat.dist[[2]]
              dist <- cov.strat.dist[[1]]
              int <- c(0.025, 0.5, 0.975)
              if(dist == "ztruncpois"){
                temp <- rtpois(999, mean = dist.param$mean)
                quantiles <- quantile(temp, int)
              }else{
                quantiles <- switch(dist,
                                    "normal" = qnorm(int, dist.param$mean, dist.param$sd),
                                    "poisson" = qpois(int, dist.param$lambda),
                                    "lognormal" = qlnorm(int, dist.param$meanlog, dist.param$sdlog))
              }
            }
            # get adjustment paramters
            if(length(cov.params) == no.strata){
              strat.param <- cov.params[strat]
            }else{
              strat.param <- cov.params[1]
            }
            scale.adjustments <- quantiles*strat.param
            # new scale params
            new.scale.params <- exp(log(scale.param.strat) + scale.adjustments)
            # generate y vals
            for(i in seq(along = new.scale.params)){
              #y[i,,strat] <- switch(object@key.function,
              #                      "hn" = hn.detect(x,new.scale.params[i]),
              #                      "hr" = hr.detect(x,new.scale.params[i], shape.param.strat),
              #                      "uf" = rep(new.scale.params[i], length(x)))
              y[i,,strat] <- switch(object@key.function,
                                    "hn" = exp(-x^2/(2*new.scale.params[i]^2)),
                                    "hr" = 1-exp(-(x/new.scale.params[i])^-shape.param.strat),
                                    "uf" = rep(new.scale.params[i], length(x)))
            }
          }
          if(param.type == "categorical"){
            llty <- seq(along = y[,1,strat]) + 1
          }else{
            llty <- c(2,1,2)
          }
          # Add detection functions
          for(i in seq(along = y[,1,strat])){
            lines(x, y[i,,strat], lty = llty[i], col = strat)
          }
        }#loop over strata
        # Add legend
        strata.names <- pop.desc@strata.names
        if(param.type == "discrete"){
          desc.ints <- "min,max"
        }else if(param.type == "continuous"){
          desc.ints <- "95%ints"
        }
        if(param.type == "categorical"){
          no.levels <- length(unique(cov.params$level))
          new.strata.names <- character(0)
          for(i in seq(along = strata.names)){
            new.strata.names <- c(new.strata.names, rep(strata.names[i], no.levels))
          }
          if(length(strata.names) > 0){
            legend.text <- paste(new.strata.names, ".", rep(unique(cov.params$level), no.strata), sep = "")
          }else{
            legend.text <- cov.params$level
          }
          ccol <- sort(rep(1:no.strata, length(scale.adjustments)))
          llty <- rep(1:length(scale.adjustments),no.strata) + 1
        }else{
          ccol <- sort(rep(1:no.strata, 2))
          llty <- rep(c(1,2), no.strata)
          new.strata.names <- character(0)
          description <- character(0)
          for(i in seq(along = strata.names)){
            new.strata.names <- c(new.strata.names, rep(strata.names[i], 2))
            description <- c(description, "mean", desc.ints)
          }
          if(length(strata.names) > 0){
            legend.text <- paste(new.strata.names, ".", description, sep = "")
          }else{
            legend.text <- c("mean", desc.ints)
          }
        }
        legend(object@truncation, 1.2,  col = ccol, lty = llty, legend = legend.text, bty = "n", box.col = "white", xjust = 1)
      }#loop over covariates
    }else{ #if there are no covariates
      # Find number of strata
      no.strata <- max(length(scale.param), length(shape.param))
      if(length(pop.desc@strata.names) > 0){
        strata.names <- pop.desc@strata.names
      }else{
        strata.names <- pop.desc@region.name
      }
      # set up initial plot
      plot(0,0, xlim = c(0,object@truncation + object@truncation*0.05), ylim = c(0,1.2), main = paste("Detection Function", cov.names[cov], sep = ""), col = "white", xlab = "Distance", ylab = "Detection Probability")
      for(strat in 1:no.strata){
        # basic scale param
        if(length(scale.param) == no.strata){
          scale.param.strat <- scale.param[strat]
        }else{
          scale.param.strat <- scale.param[1]
        }
        # if hazard rate do the same for shape parameter
        if(object@key.function == "hr"){
          if(length(shape.param) == no.strata){
            shape.param.strat <- shape.param[strat]
          }else{
            shape.param.strat <- shape.param[1]
          }
        }
        #y <- switch(object@key.function,
        #            "hn" = hn.detect(x,scale.param.strat),
        #            "hr" = hr.detect(x,scale.param.strat, shape.param.strat),
        #            "uf" = rep(scale.param.strat, length(x)))
        y <- switch(object@key.function,
                              "hn" = exp(-x^2/(2*scale.param.strat^2)),
                              "hr" = 1-exp(-(x/scale.param.strat)^-shape.param.strat),
                              "uf" = rep(scale.param.strat, length(x)))
        lines(x, y, col = strat, lwd = 2)
      }
      legend(object@truncation, 1.2,  lty = 1, lwd = 2, col = 1:no.strata, legend = strata.names, bty = "n", box.col = "white", xjust = 1)
    }
    invisible(x)
  })






