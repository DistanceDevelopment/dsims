#' @include Population.R
#' @include generic.functions.R

#' @title Virtual Class "Survey"
#'
#' @description Class \code{"Survey"} is an S4 class containing an instance of a population.
#'
#' @name Survey-class
#' @slot population Object of class \code{"Population"}; an instance of
#' a population.
#' @keywords classes
setClass("Survey", representation(population = "Population",
                                  dist.data = "data.frame",
                                  dists.in.covered = "numeric", "VIRTUAL"))

# GENERIC METHODS DEFINITIONS --------------------------------------------

#' @rdname analyse.data-methods
#' @param point logical indicating whether it is a point transect survey
#' @param warnings a list of warnings and how many times they arose
#' @export
setMethod(
  f="analyse.data",
  signature=c("Survey", "DS.Analysis"),
  definition=function(survey, analysis, warnings = list()){
    # Get distance data
    dist.data <- survey@dist.data
    # Check what kind of survey it is
    transect <- switch(class(survey),
                       Survey.LT = "line",
                       Survey.PT = "point")
    # Strip out missing distances
    #dist.data <- dist.data[!is.na(dist.data),]
    # Make sure there is a detected column
    if(is.null(dist.data$detected)){
      dist.data$detected <- rep(1, nrow(dist.data))
    }
    if(object@binned.data){
      #binned data
      dist.data <- dist.data[dist.data$distance <= max(object@cutpoints),]
      dist.data <- create.bins(dist.data, cutpoints = object@cutpoints)
      #Try to fit ddf model
      if(point){
        fit.model <- paste("ddf(dsmodel = ~", as.character(object@dsmodel)[2] ,", data = dist.data, method = 'ds', meta.data = list(width = ", max(object@cutpoints), ", point = TRUE, binned = TRUE, breaks = ", object@cutpoints ,"), control = list(silent = TRUE))", sep = "")
      }else{
        fit.model <- paste("ddf(dsmodel = ~", as.character(object@dsmodel)[2] ,", data = dist.data, method = 'ds', meta.data = list(width = ", max(object@cutpoints), ", binned = TRUE, breaks = ", object@cutpoints ,"), control = list(silent = TRUE))", sep = "")
      }
    }else{
      #exact distances
      if(length(object@truncation) == 0){
        #If there is no truncation distance specified
        if(point){
          model.fit <- paste("ddf(dsmodel = ~", as.character(object@dsmodel)[2] ,", data = dist.data, method = 'ds', meta.data = list(point = TRUE), control = list(silent = TRUE))", sep = "")
        }else{
          model.fit <- paste("ddf(dsmodel = ~", as.character(object@dsmodel)[2] ,", data = dist.data, method = 'ds', control = list(silent = TRUE))", sep = "")
        }
      }else{
        #If there is a truncation distance
        if(point){
          model.fit <- paste("ddf(dsmodel = ~", as.character(object@dsmodel)[2] ,", data = dist.data, method = 'ds', meta.data = list(point = TRUE, width = ", object@truncation,"), control = list(silent = TRUE))", sep = "")
        }else{
          model.fit <- paste("ddf(dsmodel = ~", as.character(object@dsmodel)[2] ,", data = dist.data, method = 'ds', meta.data = list(width = ", object@truncation,"), control = list(silent = TRUE))", sep = "")
        }
      }
    }

    #Set warning to NULL
    W <- NULL
    # Fit model
    ddf.result <- withCallingHandlers(tryCatch(eval(parse(text = model.fit)),
                                               error=function(e)e),
                                      warning=function(w){W <<- w; invokeRestart("muffleWarning")})
    #check if there was an error, warning or non-convergence
    if(any(class(ddf.result) == "error")){
      warnings <- message.handler(warnings, paste("Error: ", ddf.result$message, " (Model call: ", as.character(object@dsmodel)[2], ")", sep = ""))
      ddf.result <- NA
    }else if(ddf.result$ds$converge != 0){
      ddf.result <- NA
      warnings <- message.handler(warnings, paste("The following model failed to converge: ", object@dsmodel, sep = ""))
    }else if(any(predict(ddf.result)$fitted < 0)){
      ddf.result <- NA
      warnings <- message.handler(warnings, "Negative predictions, excluding these results")
    }
    if(!is.null(W)){
      warnings <- message.handler(warnings, paste(W, " (Model call: ", as.character(object@dsmodel)[2], ")", sep = ""))
    }
    return(list(ddf.result = ddf.result, warnings = warnings))
  }
)




