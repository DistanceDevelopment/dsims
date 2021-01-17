#' @include generic.functions.R
#' @include Survey.R

#' @title Class "DS.Analysis"
#'
#' @description Class \code{"DDF.Analysis"} is an S4 class describing a basic
#'  detection function model wto be fitted to distance sampling data.
#'
#' @name DS.Analysis-class
#' @title S4 Class "DS.Analysis"
#' @slot dsmodel Object of class \code{"formula"}; describing the
#'  detection function model.
#' @slot key key function to use; "hn" gives half-normal (default), "hr"
#'  gives hazard-rate and "unif" gives uniform. Note that if uniform key
#'  is used, covariates cannot be included in the model.
#' @slot adjustment a list containing adjustment parameters: adjustment -
#'  either "cos" (recommended), "herm" or "poly", order - the orders of
#'  the adjustment terms to fit, scale - the scale by which the distances
#'  in the adjustment terms are divided. See details.
#' @slot truncation Object of class \code{"character"}; Specifies
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
#' @slot fitted.models A list of the models fitted.
#' @section Methods:
#' \describe{
#'  \item{\code{run.analysis}}{\code{signature=c(object = "DS.Analysis",
#'  data = data.frame)}: runs the analysis described in the object on the
#'  data provided.}
#' }
#' @keywords classes
#' @export
setClass(Class = "DS.Analysis", representation(dsmodel = "list",
                                               key = "character",
                                               adjustment = "list",
                                               truncation = "list",
                                               cutpoints = "numeric",
                                               er.var = "character",
                                               control.opts = "list",
                                               group.strata = "data.frame",
                                               criteria = "character"))

setMethod(
  f="initialize",
  signature="DS.Analysis",
  definition=function(.Object, dsmodel, key, adjustment, truncation, cutpoints, er.var, control.opts, group.strata, criteria){
    .Object@dsmodel <- dsmodel
    .Object@key <- key
    .Object@adjustment <- adjustment
    .Object@truncation <- truncation
    .Object@cutpoints <- cutpoints
    .Object@er.var <- er.var
    .Object@control.opts <- control.opts
    .Object@group.strata <- group.strata
    .Object@criteria <- criteria
    #Check object is valid
    validObject(.Object)
    # return object
    return(.Object)
  }
)

setValidity("DS.Analysis",
            function(object){
              if(!(object@criteria %in% c("AIC", "BIC", "AICc"))){
                return("This selection criteria is not currently supported, please select from 'AIC', 'BIC' or 'AICc'.")
              }
              #make sure these are characters not factors
              object@group.strata$design.id <- as.character(object@group.strata$design.id)
              object@group.strata$analysis.id <- as.character(object@group.strata$analysis.id)
              #Check that the adjustment list is either empty or has adjustment, order and scale and are the correct length
              return(TRUE)
            }
)

# GENERIC METHODS DEFINITIONS --------------------------------------------

#' @rdname analyse.data-methods
#' @param point logical indicating whether it is a point transect survey
#' @param warnings a list of warnings and how many times they arose
#' @export
setMethod(
  f="analyse.data",
  signature=c("DS.Analysis", "Survey"),
  definition=function(analysis, data.obj, warnings = list(), ...){
    # Get distance data
    dist.data <- data.obj@dist.data
    # Check what kind of survey it is
    transect <- switch(class(survey),
                       Survey.LT = "line",
                       Survey.PT = "point")
    #Call analyse.data on model and dataset
    analysis <- analyse.data(analysis, dist.data, transect = transect)
    return(analysis)
  }
)



#' @rdname analyse.data-methods
#' @param point logical indicating whether it is a point transect survey
#' @param warnings a list of warnings and how many times they arose
#' @export
#' @importFrom Distance ds
setMethod(
  f="analyse.data",
  signature=c("DS.Analysis", "data.frame"),
  definition=function(analysis, data.obj, warnings = list(), ...){
    dist.data <- data.obj
    # deal with ... arguments
    args <- list(...)
    if(!"transect" %in% names(args)){
      #assume it is a line transect if it is missing
      transect <- "line"
    }else{
      transect <- args$transect
    }
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
      monotonicity <- ifelse(analysis@dsmodel == ~1, "strict", "none")
    }
    if("method" %in% names(analysis@control.opts)){
      method <- analysis@control.opts$method
    }else{
      method <- "nlminb"
    }
    if("initial.values" %in% names(analysis@control.opts)){
      initial.values <- analysis@control.opts$initial.values
    }else{
      initial.values <- NULL
    }
    if(length(analysis@truncation[[1]] == 1)){
      truncation <- analysis@truncation[[1]]
    }else{
      truncation <- analysis@truncation
    }
    # Data Prep
    # if binned analysis add distbegin and distend cols
    if(length(analysis@cutpoints) > 0){
      # bin data
      dist.data <- dist.data[dist.data$distance <= max(object@cutpoints),]
      dist.data <- create.bins(dist.data, cutpoints = object@cutpoints)
    }
    # Make sure there is a detected column
    #if(is.null(dist.data$detected)){
    #  dist.data$detected <- rep(1, nrow(dist.data))
    #}


    # Fit models
    models <- list()
    IC <- numeric()
    for(i in seq(along = analysis@dsmodel)){
      # Set W to null
      W <- NULL
      # Try to fit model
      models[[i]] <- suppressMessages(
        withCallingHandlers(tryCatch(Distance::ds(data = dist.data,
                                                     truncation = truncation,
                                                     transect = transect,
                                                     formula = analysis@dsmodel[[i]],
                                                     key = analysis@key[i],
                                                     adjustment = adjustment,
                                                     order = order,
                                                     scale = scale,
                                                     cutpoints = NULL, #analysis@cutpoints,
                                                     monotonicity = monotonicity,
                                                     er.var = analysis@er.var,
                                                     method = method,
                                                     initial.values = initial.values,
                                                     max.adjustments = max.adjustments),
                                                  error=function(e)e),
                                         warning=function(w){W <<- w; invokeRestart("muffleWarning")}))
      #check if there was an error, warning or non-convergence
      if(any(class(models[[i]]) == "error")){
        warnings <- message.handler(warnings, paste("Error: ", models[[i]]$message, " (Model number: ", i, ")", sep = ""))
        models[[i]] <- NA
      }else if(models[[i]]$ddf$ds$converge != 0){
        warnings <- message.handler(warnings, paste("The following model failed to converge: ", i, sep = ""))
        models[[i]] <- NA
      }else if(any(predict(models[[i]])$fitted < 0)){
        warnings <- message.handler(warnings, paste("Negative predictions for model ", i,", excluding these results."), sep = "")
        ddf.result <- NA
      }
      if(!is.null(W)){
        warnings <- message.handler(warnings, paste(W, " (Model call: ", as.character(object@dsmodel)[2], ")", sep = ""))
      }
      if(class(models[[i]]) == "dsmodel"){
        IC[i] <- switch(analysis@criteria,
                        "AIC" = AIC(models[[i]])$AIC,
                        "BIC" = BIC(models[[i]]))
      }
    } #Fit next model
    #Find model with the minimum information criteria
    if(length(IC) > 0){
      min.IC <- min(IC, na.rm = TRUE)
      index <- which(IC == min.IC)
      min.model <- models[[index]]
    }else{
      warnings <- message.handler(warnings, "None of the models converged for this dataset.")
      return(list(model = NULL, warnings = warnings))
    }
    # Return model and warnings
    return(list(model = min.model, warnings = warnings))
  }
)




