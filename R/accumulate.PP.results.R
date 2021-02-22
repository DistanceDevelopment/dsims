accumulate.PP.results <- function(simulation, results){
  simulation@results$filename <- rep(NA, length(results))
  for(i in seq(along = results)){
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
  return(simulation)
}

