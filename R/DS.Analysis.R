#' @include generic.functions.R

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
                                                criteria = "character",
                                                fitted.models = "list"))

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
              return(TRUE)
            }
)

# GENERIC METHODS DEFINITIONS --------------------------------------------

