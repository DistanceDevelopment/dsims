check.simulation <- function(object){
  # A function to validate the simulation object
  # This is in addition to further validation checks.
  # Arguments:
  #    object - object of class simulation
  # Value:
  #    the simulation object (potentially updated)
  #    or a character representing an error message.

  strata.names <- object@design@region@strata.name
  strata.no <- length(strata.names)



  # DETECTABILITY CHECKS

  # Check that the number of covariate values matches the number of strata
  detect <- object@detectability
  cov.param <- detect@cov.param
  cov.names <- names(cov.param)
  for(cov in seq(along = cov.param)){
    if(class(cov.param[[cov]]) == "data.frame"){
      # Check data.frame set up
    }else if(class(cov.param[[cov]]) == "numeric"){
      if(length(cov.param[[cov]]) == 1 && strata.no > 1){
        cov.param[[cov]] <- rep(cov.param[[cov]], strata.no)
        object@detectability@cov.param <- cov.param
      }else if(length(cov.param[[cov]]) > 1 && length(cov.param[[cov]]) != strata.no){
        return(paste("The number of covariate parameters for ", cov.names[cov], " is not the same as the number of strata. Please supply one global value or one value per stratum.", sep = ""))
      }
    }
  }



  return(object)
}
