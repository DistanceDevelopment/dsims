#' save.sim.results
#'
#' Saves the simulation results from each replicate to file. It will save up to 3 txt files, one for the abundance estimation for individuals, one for the abundance estimation of clusters (where applicable) and one for detectability estimates and model selection information.
#'
#' @param simulation object of class \code{Simulation} which has been run.
#' @param filepath optionally a path to the directory where you would like the files saved, otherwise it will save it to the working directory.
#' @param sim.ID optionally you can add a simulation ID to the filename
#' @export
#' @importFrom utils write.table
#' @rdname save.sim.results-methods
#' @return invisibly returns the original simulation object
#' @author L. Marshall
save.sim.results <- function(simulation, filepath = character(0), sim.ID = numeric(0)){
  #Individuals
  no.reps <- simulation@reps
  no.strata <- dim(simulation@results$individuals$N)[1]
  no.cols <- dim(simulation@results$individuals$N)[2] + 1
  col.names <- c("rep", dimnames(simulation@results$individuals$N)[[2]])
  results.dimnames <- list()
  results.dimnames[[1]] <- rep(dimnames(simulation@results$individuals$N)[[1]], no.reps)
  results.dimnames[[2]] <- col.names
  N.results.mat <- matrix(NA, nrow = no.reps*no.strata, ncol = no.cols, dimnames = results.dimnames)
  for(i in 1:no.reps){
    N.results.mat[(no.strata*i-(no.strata-1)):(no.strata*i), 2:no.cols] <- simulation@results$individuals$N[,,i]
    N.results.mat[(no.strata*i-(no.strata-1)):(no.strata*i), 1] <- rep(i, no.strata)
  }
  if(length(sim.ID) > 0){
    sim.ext <- paste("_sim", sim.ID, sep = "")
  }else{
    sim.ext <- ""
  }
  if(length(filepath) == 0){
    filepath <- getwd()
  }
  write.table(N.results.mat, file = paste(filepath, "/IndividualResultsN", sim.ext, ".txt", sep = ""), sep = "\t")

  #Do the same for clusters if there are any
  if(!is.null(simulation@results$clusterss$N)){
    no.cols <- dim(simulation@results$clusters$N)[2] + 1
    col.names <- c("rep", dimnames(simulation@results$individuals$N)[[2]])
    results.dimnames <- list()
    results.dimnames[[1]] <- rep(dimnames(simulation@results$clusters$N)[[1]], no.reps)
    results.dimnames[[2]] <- col.names
    N.results.mat <- matrix(NA, nrow = no.reps*no.strata, ncol = no.cols, dimnames = results.dimnames)
    for(i in 1:no.reps){
      N.results.mat[(no.strata*i-(no.strata-1)):(no.strata*i), 2:no.cols] <- simulation@results$clusters$N[,,i]
      N.results.mat[(no.strata*i-(no.strata-1)):(no.strata*i), 1] <- rep(i, no.strata)
    }
    write.table(N.results.mat, file = paste(filepath, "/ClusterResultsN", sim.ext, ".txt", sep = ""), sep = "\t")
  }

  #Save model selection results
  no.rows <- dim(simulation@results$Detection)[1]
  no.cols <- dim(simulation@results$Detection)[2] + 1
  col.names <- c("rep", dimnames(simulation@results$Detection)[[2]])
  results.dimnames <- list()
  results.dimnames[[1]] <- rep(dimnames(simulation@results$Detection)[[1]], no.reps)
  results.dimnames[[2]] <- col.names
  Det.results.mat <- matrix(NA, nrow = no.reps*no.rows, ncol = no.cols, dimnames = results.dimnames)
  for(i in 1:no.reps){
    Det.results.mat[(no.rows*i-(no.rows-1)):(no.rows*i), 2:no.cols] <- simulation@results$Detection[,,i]
    Det.results.mat[(no.rows*i-(no.rows-1)):(no.rows*i), 1] <- rep(i, no.rows)
  }
  write.table(Det.results.mat, file = paste(filepath, "/DetectionResults", sim.ext, ".txt", sep = ""), sep = "\t")

  #Nothing to return
  invisible(simulation)
}


