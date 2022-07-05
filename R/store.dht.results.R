store.dht.results <- function(results, dht.results, i, clusters, data, obs.tab, sample.tab){
  # Calculate the number of observations with missing distances
  strata.names <- dimnames(results$individuals$summary)[[1]]
  strata.names.ND <- as.character(dht.results$individual$N$Label)
  # To calculate k for individuals
  sampler.count <- table(sample.tab$Region.Label)
  if("Total" %in% strata.names && length(strata.names) > 1){
    sampler.count <- c(sampler.count, sum(sampler.count))
    last <- length(sampler.count)
    names(sampler.count)[last] <- "Total"
  }
  for(strat in seq(along = strata.names)){
    # Get the number of missed dists in strata
    if(strata.names[strat] == "Total"){
      object.ids <- obs.tab$object
    }else{
      object.ids <- obs.tab$object[obs.tab$Region.Label == strata.names[strat]]
    }
    strata.data <- data[data$object %in% object.ids,]
    data.miss.dists <- strata.data[is.na(strata.data),]
    data.miss.dists <- data.miss.dists[!is.na(data.miss.dists$object),]
    if(clusters){
      # Need to sum cluster sizes
      results$individuals$summary[strat,c("n.miss.dist"),i] <- sum(data.miss.dists$size)
      results$clusters$summary[strat,c("n.miss.dist"),i] <- nrow(data.miss.dists)
    }else{
      results$individuals$summary[strat,c("n.miss.dist"),i] <- nrow(data.miss.dists)
    }
    results$individuals$summary[strat,c("Area", "CoveredArea", "Effort", "n", "ER", "se.ER", "cv.ER"),i] <- as.matrix(dht.results$individuals$summary[dht.results$individual$summary$Region == strata.names[strat],c("Area", "CoveredArea", "Effort", "n", "ER", "se.ER", "cv.ER")])
    # Add k
    results$individuals$summary[strat,c("k"),i] <- sampler.count[[strata.names[strat]]]
    results$individuals$N[strat,c("Estimate", "se", "cv", "lcl", "ucl", "df"),i] <- as.matrix(dht.results$individuals$N[dht.results$individual$N$Label == strata.names.ND[strat],c("Estimate", "se", "cv", "lcl", "ucl", "df")])
    results$individuals$D[strat,c("Estimate", "se", "cv", "lcl", "ucl", "df"),i] <- as.matrix(dht.results$individuals$D[dht.results$individual$D$Label == strata.names.ND[strat],c("Estimate", "se", "cv", "lcl", "ucl", "df")])
  }
  if(clusters){
    for(strat in seq(along = strata.names)){
      results$clusters$summary[strat,c("Area", "CoveredArea", "Effort", "n", "k", "ER", "se.ER", "cv.ER"),i] <- as.matrix(dht.results$clusters$summary[dht.results$clusters$summary$Region == strata.names[strat],c("Area", "CoveredArea", "Effort", "n", "k", "ER", "se.ER", "cv.ER")])
      results$clusters$N[strat,c("Estimate", "se", "cv", "lcl", "ucl", "df"),i] <- as.matrix(dht.results$clusters$N[dht.results$clusters$N$Label == strata.names.ND[strat],c("Estimate", "se", "cv", "lcl", "ucl", "df")])
      results$clusters$D[strat,c("Estimate", "se", "cv", "lcl", "ucl", "df"),i] <- as.matrix(dht.results$clusters$D[dht.results$clusters$D$Label == strata.names.ND[strat],c("Estimate", "se", "cv", "lcl", "ucl", "df")])
      # Needed to add and as.data.frame as this table had turned into a list at some point
      results$expected.size[strat, c("Expected.S","se.Expected.S"), i] <- as.matrix(as.data.frame(dht.results$Expected.S)[dht.results$Expected.S$Region == strata.names.ND[strat], c("Expected.S","se.Expected.S")])[1,]
    }
  }
  return(results)
}
