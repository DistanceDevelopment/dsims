#' Provides a description of the summary object/output
#'
#' Prints a list of the terms used in the simulation summary.
#'
#' @export
#' @return no return, displays an explanation of the simulation summary
#' @author Laura Marshall
#'
description.summary <- function(){
  cat("\nGLOSSARY")
  cat("\n--------\n")
  cat("\nSummary of Simulation Output\n")
  cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
  cat("\nRegion          : the region name.")
  cat("\nNo. Repetitions : the number of times the simulation was repeated.")
  cat("\nNo. Excluded Repetitions : the number of times the simulation failed
                  (too few sightings, model fitting failure etc.)\n")

  cat("\nSummary for Individuals\n")
  cat("~~~~~~~~~~~~~~~~~~~~~~~\n")
  cat("\nSummary Statistics:")
  cat("\n   mean.Cover.Area : mean covered across simulation.")
  cat("\n   mean.Effort     : mean effort across simulation.")
  cat("\n   mean.n          : mean number of observed objects across
                     simulation.")
  cat("\n   mean.n.miss.dist: mean number of observed objects where no distance
                    was recorded (only displayed if value > 0).")
  cat("\n   no.zero.n       : number of surveys in simulation where
                     nothing was detected (only displayed if value > 0).")
  cat("\n   mean.ER         : mean encounter rate across simulation.")
  cat("\n   mean.se.ER      : mean standard error of the encounter rates
                     across simulation.")
  cat("\n   sd.mean.ER      : standard deviation of the encounter rates
                     across simulation.")

  cat("\n\nEstimates of Abundance:")
  cat("\n   Truth            : true population size, (or mean of true
                      population sizes across simulation for Poisson N.")
  cat("\n   mean.Estimate    : mean estimate of abundance across simulation.")
  cat("\n   percent.bias     : the percentage of bias in the estimates.")
  cat("\n   RMSE             : root mean squared error/no. successful reps")
  cat("\n   CI.coverage.prob : proportion of times the 95% confidence interval
                      contained the true value.")
  cat("\n   mean.se          : the mean standard error of the estimates of
                      abundance")
  cat("\n   sd.of.means      : the standard deviation of the estimates")

  cat("\n\nEstimates of Density:")
  cat("\n   Truth            : true average density.")
  cat("\n   mean.Estimate    : mean estimate of density across simulation.")
  cat("\n   percent.bias     : the percentage of bias in the estimates.")
  cat("\n   RMSE             : root mean squared error/no. successful reps")
  cat("\n   CI.coverage.prob : proportion of times the 95% confidence interval
                      contained the true value.")
  cat("\n   mean.se          : the mean standard error of the estimates.")
  cat("\n   sd.of.means      : the standard deviation of the estimates.")

  cat("\n\nDetection Function Values\n")
  cat("~~~~~~~~~~~~~~~~~~~~~~~~~\n")
  cat("\n mean.observed.Pa : mean proportion of individuals/clusters observed in
                    the covered region.")
  cat("\n mean.estimte.Pa  : mean estimate of the proportion of individuals/
                    clusters observed in the covered region.")
  cat("\n sd.estimate.Pa   : standard deviation of the mean estimates of the
                    proportion of individuals/clusters observed in the
                    covered region.")
  cat("\n mean.ESW         : mean estimated strip width.")
  cat("\n sd.ESW           : standard deviation of the mean estimated strip widths.")

}
