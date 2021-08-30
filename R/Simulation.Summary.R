#' @title Class "Simulation.Summary"
#'
#' @description Class \code{"Simulation.Summary"} is an S4 class containing a summary of
#' the simulation results. This is returned when \code{summary(Simulation)}
#' is called. If it is not assigned to a variable the object will be
#' displayed via the \code{show} method.
#'
#' @name Simulation.Summary-class
#' @title S4 Class "Simulation.Summary"
#' @docType class
#' @keywords classes
#' @section Methods:
#' \describe{
#'  \item{\code{show}}{\code{signature=(object = "Simulation.Summary")}: prints
#'  the contents of the object in a user friendly format.}
#'  }
setClass("Simulation.Summary", representation(region.name = "character",
                                              strata.name = "character",
                                              total.reps = "numeric",
                                              failures = "numeric",
                                              use.max.reps = "logical",
                                              individuals = "list",
                                              clusters = "list",
                                              expected.size = "data.frame",
                                              population.covars = "list",
                                              detection = "data.frame",
                                              model.selection = "table",
                                              design.summary = "list",
                                              detectability.summary = "list",
                                              analysis.summary = "list",
                                              include.glossary = "logical"))


#' @importFrom methods validObject
setMethod(
  f="initialize",
  signature="Simulation.Summary",
  definition=function(.Object, region.name, strata.name, total.reps, failures, use.max.reps, individuals, clusters = list(), expected.size = data.frame(NULL), population.covars, detection, model.selection, design.summary = list(), detectability.summary = list(), analysis.summary = list(), include.glossary = FALSE){
    #Set slots
    .Object@region.name     <- region.name
    .Object@strata.name     <- strata.name
    .Object@total.reps      <- total.reps
    .Object@failures        <- failures
    .Object@use.max.reps   <- use.max.reps
    .Object@individuals     <- individuals
    .Object@clusters        <- clusters
    .Object@expected.size   <- expected.size
    .Object@population.covars <- population.covars
    .Object@detection       <- detection
    .Object@model.selection <- model.selection
    .Object@design.summary  <- design.summary
    .Object@detectability.summary <- detectability.summary
    .Object@analysis.summary <- analysis.summary
    .Object@include.glossary <- include.glossary
    #Check object is valid
    validObject(.Object)
    # return object
    return(.Object)
  }
)

setValidity("Simulation.Summary",
            function(object){
              return(TRUE)
            }
)

################################################################################
# GENERIC METHODS
################################################################################

#' show
#'
#' Displays the simulation summary
#'
#' @param object object of class Simulation.Summary
#' @return No return value, displays information in Simulation.Summary
#' object
#' @rdname show.Simulation.Summary-methods
#' @export
setMethod(
  f="show",
  signature="Simulation.Summary",
  definition=function(object){
    # Check if it has been run but all reps failed
    run.no.results <- object@failures > 0 && object@failures == object@total.reps
    # Check if it has been run yet
    not.run <- all(is.na(object@individuals$summary)) && !run.no.results
    #Get strata names
    strata.names <- object@strata.name
    #Display summaries
    cat("\n\nRegion: ", object@region.name)
    cat("\nNo. Repetitions: ", object@total.reps)
    if(not.run){
      cat("\nNo. Excluded Repetitions: NA", fill = TRUE)
    }else{
      cat("\nNo. Excluded Repetitions: ", object@failures, fill = TRUE)
    }
    use.max.reps <- object@use.max.reps
    if(use.max.reps){
      cat("Using all repetitions where at least one model converged.", fill = TRUE)
    }else{
      cat("Using only repetitions where all models converged.", fill = TRUE)
    }
    cat("\nDesign: ", object@design.summary$design.type, fill = TRUE)
    index <- which(names(object@design.summary) != "design.type")
    for(i in seq(along = index)){
      cat("  ", names(object@design.summary)[i], ": ", object@design.summary[[i]], fill = TRUE)
    }
    # Display population covariate info
    cov.names <- names(object@population.covars)
    if(length(cov.names) > 0){
      cat("\nIndividual Level Covariate Summary:")
    }
    for(i in seq(along = cov.names)){
      temp <- object@population.covars[[cov.names[i]]]
      if (length(temp) == 1) {
        cat("\n   ", cov.names[i], ":", sep = "")
        if(class(temp[[1]]) == "data.frame"){
          cat("\n")
          print(temp[[1]], row.names = FALSE)
        }else{
          dist <- temp[[1]][[1]]
          param.names <- names(temp[[1]][[2]])
          param.values <- unlist(temp[[1]][[2]])
          cat(dist, ", ")
          cat(param.names[1], " = ", param.values[1])
          if(length(param.names) > 1){
            for(k in 2:length(param.names)){
              cat(", ", param.names[k], " = ", param.values[k])
            }
          }
        }
      } else{
        cat("\n   ", cov.names[i], ":", sep = "")
        for (j in seq(along = temp)) {
          if(class(temp[[j]]) == "data.frame"){
            if(j == 1){
              cat("\n")
            }
            cat("Strata ", strata.names[j], ": \n", sep = "")
            print(temp[[j]], row.names = FALSE)
          }else{
            dist <- temp[[j]][[1]]
            param.names <- names(temp[[j]][[2]])
            param.values <- unlist(temp[[j]][[2]])
            cat("\n      Strata ", strata.names[j], ": ", dist, ", ", sep = "")
            cat(param.names[1], " = ", param.values[1], sep = "")
            if(length(param.names) > 1){
              for(k in 2:length(param.names)){
                cat(", ", param.names[k], " = ", param.values[k])
              }
            }
          }
        }
      }
    }

    cat("\nPopulation Detectability Summary:", fill = TRUE)
    for(i in seq(along = object@detectability.summary)){
      if(length(object@detectability.summary[[i]]) > 0 & names(object@detectability.summary)[i] != "cov.param"){
        cat("   ",names(object@detectability.summary)[i], " = ", object@detectability.summary[[i]], fill = TRUE)
      }
    }
    detect.cov.names <- names(object@detectability.summary$cov.param)
    if(length(detect.cov.names) > 0){
      cat("\nCovariate Detectability Summary (params on log scale):", fill = TRUE)
      for(i in seq(along = detect.cov.names)){
        cat("   ", detect.cov.names[i], " parameters: \n", sep = "")
        temp <- object@detectability.summary$cov.param[[detect.cov.names[i]]]
        if(class(temp) == "data.frame"){
          print(temp, row.names = FALSE)
        }else{
          names(temp) <- paste("Strata ", strata.names, sep = "")
          print(temp)
        }
      }
    }
    cat("\nAnalysis Summary:")
    for(i in seq(along = object@analysis.summary)){
      if(names(object@analysis.summary)[i] == "dfmodels"){
        cat("\n   Candidate Models:", fill = TRUE)
        for(j in seq(along = object@analysis.summary$dfmodels)){
          no.times.selected <- object@model.selection[names(object@model.selection) == as.character(j)]
          if(length(no.times.selected) == 0){
            no.times.selected <- 0
          }
          cat("      Model ", j, ": ", "key function '", object@analysis.summary$key[j], "', formula '", as.character(object@analysis.summary$dfmodels[[j]]), "', was selected ", no.times.selected, " time(s).", sep = "", fill = TRUE)
        }
      }else if(names(object@analysis.summary)[i] %in% c("criteria", "variance.estimator")){
        cat("  ",names(object@analysis.summary)[i], " = ", object@analysis.summary[[i]], fill = TRUE)
      }else if(names(object@analysis.summary)[i] %in% c("truncation")){
        cat("  ",names(object@analysis.summary)[i], " = ", unlist(object@analysis.summary[[i]]), fill = TRUE)
      }
    }
    # Check to see if simulation has been run
    if(!not.run && !run.no.results){
      cat("\nSummary for Individuals")
      cat("\n\nSummary Statistics\n\n")
      print(object@individuals$summary)
      cat("\n     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      cat("\nEstimates of Abundance (N)\n\n")
      print(round(object@individuals$N,2))
      cat("\n     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      cat("\nEstimates of Density (D)\n\n")
      print(object@individuals$D)
      cat("\n     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      cat("\n     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      if(length(object@clusters) > 0){
        cat("\n\nSummary for Clusters")
        cat("\n\nSummary Statistics\n\n")
        print(object@clusters$summary)
        cat("\n     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
        cat("\nEstimates of Abundance (N)\n\n")
        print(round(object@clusters$N,2))
        cat("\n     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
        cat("\nEstimates of Density (D)\n\n")
        print(object@clusters$D)
        cat("\n     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
        cat("\nEstimates of Expected Cluster Size\n\n")
        print(round(object@expected.size,2))
        cat("\n     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
        cat("\n     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      }
      cat("\n\nDetection Function Values\n\n")
      print(round(object@detection,2))
    }else if(not.run){
      cat("\n No results to display yet... simulation has not been run.")
    }else if(run.no.results){
      cat("\n There were no successful repetitions.")
      if(!use.max.reps){
        cat("\n\n Note: use.max.reps is false so only repetitions where all models converged have been included in the summary. This option can be changed when calling the summary function and does not involve re-running the simulation. See ?`summary,Simulation-method`")
      }
    }
  }
)

