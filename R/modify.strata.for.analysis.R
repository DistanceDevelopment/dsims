modify.strata.for.analysis <- function(analysis.strata, obs.table, sample.table, region.table){
  #This function modifies the strata ID's so you can select a different
  #stratification for analysis than for design

  #New Label fields
  obs.RL <- as.character(obs.table$Region.Label)
  sample.RL <- as.character(sample.table$Region.Label)
  region.RL <- as.character(region.table$Region.Label)

  analysis.strata$design.id <- as.character(analysis.strata$design.id)
  analysis.strata$analysis.id <- as.character(analysis.strata$analysis.id)

  #Update tables
  for(i in seq(along = analysis.strata$design.id)){
    obs.RL <- ifelse(obs.RL == analysis.strata$design.id[i], analysis.strata$analysis.id[i], obs.RL)
    sample.RL <- ifelse(sample.RL == analysis.strata$design.id[i], analysis.strata$analysis.id[i], sample.RL)
    region.RL <- ifelse(region.RL == analysis.strata$design.id[i], analysis.strata$analysis.id[i], region.RL)
  }

  #Replace in tables
  obs.table$Region.Label <- obs.RL
  sample.table$Region.Label <- sample.RL
  region.table$Region.Label <- region.RL

  #Now join strata in region.table
  new.region.table <- data.frame(Region.Label = unique(region.table$Region.Label))
  new.Area <- rep(NA, nrow(new.region.table))
  for(i in seq(along = new.region.table$Region.Label)){
    new.Area[i] <- sum(region.table$Area[region.table$Region.Label == new.region.table$Region.Label[i]])
  }
  new.region.table$Area <- new.Area
  region.table <- new.region.table

  #Return tables in a list
  return(list(obs.table = obs.table, sample.table = sample.table, region.table = region.table))

}
