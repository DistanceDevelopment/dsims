#' @include generic.functions.R
#' @include Survey.R

#' @title Class "DS.Analysis"
#'
#' @description Class \code{"DDF.Analysis"} is an S4 class describing a basic
#'  detection function model to be fitted to distance sampling data.
#'
#' @name DS.Analysis-class
#' @title S4 Class "DS.Analysis"
#' @slot dfmodel Object of class \code{"formula"}; describing the
#'  detection function model.
#' @slot key key function to use; "hn" gives half-normal (default), "hr"
#'  gives hazard-rate and "unif" gives uniform. Note that if uniform key
#'  is used, covariates cannot be included in the model.
#' @slot adjustment a list containing adjustment parameters: adjustment -
#'  either "cos" (recommended), "herm" or "poly", order - the orders of
#'  the adjustment terms to fit, scale - the scale by which the distances
#'  in the adjustment terms are divided. See details.
#' @slot truncation Object of class \code{"list"}; Specifies
#'  the truncation distance for the analyses.
#' @slot cutpoints Object of class \code{"character"}; gives the
#'  cutpoints of the bins for binned data analysis.
#' @slot er.var specifies which encounter rate variance estimator to use.
#' @slot control.opts A list to specify various options including
#'  monotonicity, method, initial.values.
#' @slot group.strata Dataframe with two columns ("design.id" and
#' "analysis.id"). The former gives the strata names as defined in the
#' design (i.e. the region object) the second specifies how they should
#' be grouped (into less strata) for the analyses
#' @slot criteria Object of class \code{"character"}; describes
#'  which model selection criteria to use ("AIC","AICc","BIC").
#' @section Methods:
#' \describe{
#'  \item{\code{run.analysis}}{\code{signature=c(object = "DS.Analysis",
#'  data = data.frame)}: runs the analysis described in the object on the
#'  data provided.}
#' }
#' @keywords classes
#' @export
setClass(Class = "DS.Analysis", representation(dfmodel = "list",
                                               key = "character",
                                               adjustment = "list",
                                               truncation = "list",
                                               cutpoints = "numeric",
                                               er.var = "character",
                                               control.opts = "list",
                                               group.strata = "data.frame",
                                               criteria = "character"))

#' @importFrom methods validObject
setMethod(
  f="initialize",
  signature="DS.Analysis",
  definition=function(.Object, dfmodel, key, adjustment = list(), truncation, cutpoints, er.var, control.opts, group.strata, criteria){
    # Pre-processing
    #make sure these are characters not factors
    group.strata$design.id <- as.character(group.strata$design.id)
    group.strata$analysis.id <- as.character(group.strata$analysis.id)
    # Make object
    .Object@dfmodel <- dfmodel
    .Object@key <- key
    .Object@adjustment <- adjustment
    .Object@truncation <- truncation
    .Object@cutpoints <- cutpoints
    .Object@er.var <- er.var
    .Object@control.opts <- control.opts
    .Object@group.strata <- group.strata
    .Object@criteria <- criteria
    #Check object is valid
    valid <- validObject(.Object, test = TRUE)
    if(class(valid) == "character"){
      stop(paste(valid), call. = FALSE)
    }
    # return object
    return(.Object)
  }
)

setValidity("DS.Analysis",
            function(object){
              # Check criteria
              if(!(object@criteria %in% c("AIC", "BIC", "AICc"))){
                return("This selection criteria is not currently supported, please select from 'AIC', 'BIC' or 'AICc'.")
              }
              if(!all(object@key %in% c("hr", "hn"))){
                return("All key function values should be either 'hn' or 'hr'.")
              }
              # Check truncation
              if(any(unlist(lapply(object@truncation,is.character))) &&
                 (length(object@cutpoints) > 0)){
                return("Truncation cannot be supplied as a percentage with binned data.")
              }
              if("monotonicity" %in% names(object@control.opts)){
                if(!all(object@control.opts$monotonicity %in% c("none", FALSE, "weak", "strict"))){
                  return("monotonicity must be one of 'none', FALSE, 'weak' or 'strict'.")
                }
              }
              #Check that the adjustment list is either empty or has adjustment, order and scale and are the correct length
              # Check the error variance estimator
              er.var.estimators <- c("R2","R3","R4","S1","S2","O1","O2","O3","P2","P3")
              if(!(object@er.var %in% er.var.estimators)){
                return(paste("The er.var argument must be one of: '", paste(er.var.estimators, collapse = "', '"), "'.",  sep = ""))
              }
              if(!all(names(object@control.opts) %in% c("method"))){
                warning("Additional values provided to control options will be ignored. Only method is currently implemented.", immediate. = TRUE, call. = FALSE)
              }
              return(TRUE)
            }
)

# GENERIC METHODS DEFINITIONS --------------------------------------------

#' @rdname analyse.data-methods
#' @param warnings a list of warnings and how many times they arose
#' @export
setMethod(
  f="analyse.data",
  signature=c("DS.Analysis", "Survey"),
  definition=function(analysis, data.obj, warnings = list(), ...){
    # Pass in simulation repetition number for warnings
    args <- list(...)
    if("i" %in% names(args)){
      i <- args$i
    }else{
      i <- numeric(0)
    }
    # Get distance data
    dist.data <- data.obj@dist.data
    # Check what kind of survey it is
    transect <- switch(class(data.obj),
                       Survey.LT = "line",
                       Survey.PT = "point")
    #Call analyse.data on model and dataset
    analysis <- analyse.data(analysis, dist.data, transect = transect, warnings = warnings, i = i)
    return(analysis)
  }
)

#' @rdname analyse.data-methods
#' @param warnings a list of warnings and how many times they arose
#' @param transect character value either "line" or "point" specifying type of
#' transect used in survey
#' @export
#' @importFrom Distance ds
#' @importFrom stats AIC BIC
setMethod(
  f="analyse.data",
  signature=c("DS.Analysis", "data.frame"),
  definition=function(analysis, data.obj, warnings = list(), transect = "line", ...){
    # Pass in simulation repetition number for warnings
    args <- list(...)
    if("i" %in% names(args)){
      rep <- args$i
    }else{
      rep <- numeric(0)
    }
    dist.data <- data.obj
    # deal with adjustment arguments
    if(length(analysis@adjustment) == 0){
      adjustment <- NULL
      order <- NULL
      scale <- NULL
      max.adjustments <- 0
    }else{
      adjustment <- analysis@adjustment$adjustment
      order <- analysis@adjustment$order
      scale <- analysis@adjustment$scale
      max.adjustments <- analysis@adjustment$max.adjustments
    }
    # deal with control arguments
    if("monotonicity" %in% names(analysis@control.opts)){
      monotonicity <- analysis@control.opts$monotonicity
    }else{
      monotonicity <- character()
      for(i in seq(along = analysis@dfmodel)){
        monotonicity[i] <- ifelse(analysis@dfmodel[i] == ~1, "strict", "none")
      }
    }
    if("method" %in% names(analysis@control.opts)){
      method <- analysis@control.opts$method
    }else{
      method <- "nlminb"
    }
    if(length(analysis@truncation[[1]] == 1)){
      truncation <- analysis@truncation[[1]]
    }else{
      truncation <- analysis@truncation
    }
    if(length(analysis@cutpoints) == 0){
      cutpoints <- NULL
    }else{
      cutpoints <- analysis@cutpoints
    }
    # Fit models
    models <- list()
    IC <- numeric()
    for(i in seq(along = analysis@dfmodel)){
      # Set W to null
      W <- NULL
      # Try to fit model
      models[[i]] <- suppressMessages(
        withCallingHandlers(tryCatch(Distance::ds(data = dist.data,
                                                     truncation = truncation,
                                                     transect = transect,
                                                     formula = analysis@dfmodel[[i]],
                                                     key = analysis@key[i],
                                                     adjustment = adjustment[i],
                                                     order = order[i],
                                                     scale = scale[i],
                                                     cutpoints = cutpoints,
                                                     monotonicity = monotonicity[i],
                                                     er.var = analysis@er.var,
                                                     method = method,
                                                     max.adjustments = max.adjustments),
                                                  error=function(e)e),
                                         warning=function(w){W <<- w; invokeRestart("muffleWarning")}))
      #check if there was an error, warning or non-convergence
      if(any(class(models[[i]]) == "error")){
        warnings <- message.handler(warnings, paste("Error: ", models[[i]]$message, " (Model number: ", i, ")", sep = ""), rep)
        models[[i]] <- NA
      }else if(models[[i]]$ddf$ds$converge != 0){
        warnings <- message.handler(warnings, paste("The following model failed to converge: ", i, sep = ""), rep)
        models[[i]] <- NA
      }else if(any(models[[i]]$fitted < 0)){
        warnings <- message.handler(warnings, paste("Negative predictions for model ", i,", excluding these results.", sep = ""), rep)
        models[[i]] <- NA
      }else if(is.null(models[[i]]$dht)){
        warnings <- message.handler(warnings, paste("NULL value for dht part of model ", i,", excluding these results.", sep = ""), rep)
        models[[i]] <- NA
      }
      if(!is.null(W)){
        warnings <- message.handler(warnings, paste(W, " (Model number: ", i, ")", sep = ""), rep)
      }
      if(any(class(models[[i]]) == "dsmodel")){
        IC[i] <- switch(analysis@criteria,
                        "AIC" = AIC(models[[i]])$AIC,
                        "AICc" = AICc(models[[i]]),
                        "BIC" = BIC(models[[i]]))
      }else{
        IC[i] <- NA
      }
      if(!is.na(IC[i])){
        if(IC[i] == -Inf){
          IC[i] <- NA
          warnings <- message.handler(warnings, paste("The following model had model selection criteria of -Inf: ", i, sep = ""), rep)
          models[[i]] <- NA
        }
      }
    } #Fit next model
    #Find model with the minimum information criteria
    if(length(na.omit(IC)) > 0){
      min.IC <- min(IC, na.rm = TRUE)
      index <- which(IC == min.IC)
      min.model <- models[[index]]
      min.model$ddf$model.index <- index
      num.successful.models = length(which(!is.na(IC)))
      # If more than one model converged then calculate the difference in selection criteria
      # Between two best fitting models
      if(num.successful.models > 1){
        sorted.criteria <- sort(na.omit(IC))
        delta.criteria <- sorted.criteria[2] - sorted.criteria[1]
        min.model$ddf$delta.criteria <- delta.criteria
      }
    }else{
      warnings <- message.handler(warnings, "None of the models converged for this dataset.", rep)
      return(list(model = NULL, warnings = warnings, num.successful.models = 0))
    }
    # Return model and warnings
    return(list(model = min.model, warnings = warnings, num.successful.models = num.successful.models))
  }
)




