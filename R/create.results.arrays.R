create.results.arrays <- function(reps, region, ds.analysis, population.description, population.generator = NULL){
  # Make the results arrays and store in a list
  no.strata <- ifelse(length(region@strata.name) > 1, length(region@strata.name)+1, 1)
  # Check to see if the strata are grouped in the analyses
  new.strata.names <- NULL
  if(nrow(ds.analysis@group.strata) > 0){
    new.strata.names <- unique(ds.analysis@group.strata$analysis.id)
  }else{
    new.strata.names <- NULL
  }
  if(length(region@strata.name) > 1){
    if(!is.null(new.strata.names)){
      if(length(new.strata.names) > 1){
        strata.name <- c(sort(new.strata.names), "Total")
        no.strata <- length(strata.name)
      }else{
        strata.name <- new.strata.names
        no.strata <- 1
      }
    }else{
      strata.name <- c(sort(region@strata.name), "Total")
    }
  }else{
    strata.name <- region@region.name
  }
  individuals <- list(summary = array(NA, dim = c(no.strata, 9, reps+2), dimnames = list(strata.name, c("Area", "CoveredArea", "Effort", "n", "n.miss.dist", "k", "ER", "se.ER", "cv.ER"), c(1:reps,"mean","sd"))),
                      N = array(NA, dim = c(no.strata, 6, reps+2), dimnames = list(strata.name, c("Estimate", "se", "cv", "lcl", "ucl", "df"), c(1:reps,"mean","sd"))),
                      D = array(NA, dim = c(no.strata, 6, reps+2), dimnames = list(strata.name, c("Estimate", "se", "cv", "lcl", "ucl", "df"), c(1:reps,"mean", "sd"))),
                      # Realised number of individuals in the design region
                      true.N = array(NA, dim = c(no.strata, 1, reps+2), dimnames = list(strata.name, "N", c(1:reps,"mean","sd"))))
  # ZeroDetections flags repetitions recorded as zero (zero.n = "zero")
  detection = array(NA, dim = c(1, 8, reps+2), dimnames = list("Pooled", c("True.Pa", "Pa", "ESW", "f(0)", "SelectedModel", "DeltaCriteria", "SuccessfulModels", "ZeroDetections"), c(1:reps,"mean","sd")))
  #create additional arrays if animals are in clusters
  if(population.description@size){
    clusters <- list(summary = array(NA, dim = c(no.strata, 9, reps+2), dimnames = list(strata.name, c("Area", "CoveredArea", "Effort", "n", "n.miss.dist", "k", "ER", "se.ER", "cv.ER"), c(1:reps,"mean","sd"))),
                     N = array(NA, dim = c(no.strata, 6, reps+2), dimnames = list(strata.name, c("Estimate", "se", "cv", "lcl", "ucl", "df"), c(1:reps,"mean","sd"))),
                     D = array(NA, dim = c(no.strata, 6, reps+2), dimnames = list(strata.name, c("Estimate", "se", "cv", "lcl", "ucl", "df"), c(1:reps,"mean", "sd"))),
                     true.N = array(NA, dim = c(no.strata, 1, reps+2), dimnames = list(strata.name, "N", c(1:reps,"mean","sd"))))
    expected.size <- array(NA, dim = c(no.strata, 3, reps+2), dimnames = list(strata.name, c("Expected.S", "se.Expected.S", "cv.Expected.S"), c(1:reps,"mean","sd")))
    results <- list(individuals = individuals, clusters = clusters, expected.size = expected.size, Detection = detection)
  }else{
    results <- list(individuals = individuals, Detection = detection)
  }
  # Truth for the whole target area supplied by a population generator
  if(!is.null(population.generator)){
    results$truth <- array(NA, dim = c(1, 2, reps+2), dimnames = list("Total", c("N", "D"), c(1:reps,"mean","sd")))
  }
  return(results)
}
