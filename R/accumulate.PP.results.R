extract.rep.result <- function(results, i){
  rep.result <- list(
    individuals = list(
      summary = results$individuals$summary[,,i, drop = FALSE],
      N = results$individuals$N[,,i, drop = FALSE],
      D = results$individuals$D[,,i, drop = FALSE]
    ),
    Detection = results$Detection[,,i, drop = FALSE]
  )

  if(!is.null(results$clusters)){
    rep.result$clusters <- list(
      summary = results$clusters$summary[,,i, drop = FALSE],
      N = results$clusters$N[,,i, drop = FALSE],
      D = results$clusters$D[,,i, drop = FALSE]
    )
    rep.result$expected.size <- results$expected.size[,,i, drop = FALSE]
  }

  rep.result
}

apply.rep.result <- function(results, rep.result, i){
  results$individuals$summary[,,i] <- rep.result$individuals$summary[,,1]
  results$individuals$N[,,i] <- rep.result$individuals$N[,,1]
  results$individuals$D[,,i] <- rep.result$individuals$D[,,1]
  results$Detection[,,i] <- rep.result$Detection[,,1]
  if(!is.null(results$clusters) && !is.null(rep.result$clusters)){
    results$clusters$summary[,,i] <- rep.result$clusters$summary[,,1]
    results$clusters$N[,,i] <- rep.result$clusters$N[,,1]
    results$clusters$D[,,i] <- rep.result$clusters$D[,,1]
    results$expected.size[,,i] <- rep.result$expected.size[,,1]
  }
  results
}

accumulate.PP.results <- function(simulation, results){
  simulation@results$filename <- rep(NA, length(results))
  for(i in seq(along = results)){
    if(!is.null(results[[i]]$rep.result)){
      simulation@results <- apply.rep.result(simulation@results,
                                             results[[i]]$rep.result,
                                             i)
      if(!is.null(results[[i]]$filename) && length(results[[i]]$filename) > 0){
        simulation@results$filename[i] <- results[[i]]$filename
      }
    }else{
      # Backward-compatible path for legacy full-object result returns
      simulation@results$individuals$summary[,,i] <- results[[i]]$individuals$summary[,,i]
      simulation@results$individuals$N[,,i] <- results[[i]]$individuals$N[,,i]
      simulation@results$individuals$D[,,i] <- results[[i]]$individuals$D[,,i]
      simulation@results$Detection[,,i] <- results[[i]]$Detection[,,i]
      if(!is.null(simulation@results$clusters)){
        simulation@results$clusters$summary[,,i] <- results[[i]]$clusters$summary[,,i]
        simulation@results$clusters$N[,,i] <- results[[i]]$clusters$N[,,i]
        simulation@results$clusters$D[,,i] <- results[[i]]$clusters$D[,,i]
        simulation@results$expected.size[,,i] <- results[[i]]$expected.size[,,i]
      }
    }
  }
  return(simulation)
}

