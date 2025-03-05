simulation.consistency.check <- function(sim){
  # This function checks for consistency across objects within the simulation
  
  # TRUNCATION CHECKS
  # The truncation distances should be the same for design and detectability
  dd.trunc.match <- ifelse(sim@design@truncation == sim@detectability@truncation, TRUE, FALSE)
  analysis.trunc.greater <- ifelse(sim@ds.analysis@truncation[[1]] > sim@detectability@truncation, TRUE, FALSE)
  
  # If there is a design/detect truncation mismatch but analysis truncation distance ok
  if(!dd.trunc.match && !analysis.trunc.greater){
    warning(paste("Truncation distance for design and detectability differ, updating design truncation to be ", sim@detectability@truncation, ".", sep = ""), immediate. = TRUE, call. = FALSE)
    sim@design@truncation <- sim@detectability@truncation
    # If design/detect truncations match but analysis truncation is larger
  }else if(dd.trunc.match && analysis.trunc.greater){
    warning("Truncation distance for analysis is larger than for design/detectability this may introduce bias!", immediate. = TRUE, call. = FALSE)
    # If there is both design/detect mismatch and analysis truncation is larger
  }else if(!dd.trunc.match && analysis.trunc.greater){
    warning(paste("Truncation distance for design and detectability differ, updating design truncation to be ", sim@detectability@truncation, ". In addition, analysis truncation is greater than ", sim@detectability@truncation, " this may introduce bias!", sep = ""), immediate. = TRUE, call. = FALSE)
    sim@design@truncation <- sim@detectability@truncation
  }
  
  # Return object with any updates
  return(sim)
}