check.covariates <- function(covariate.list, no.strata){
  # check that each element has a name
  list.names <- names(covariate.list)
  if(any(list.names == "")){
    stop("Not all the elements of the covariate list are named. Please name all elements.", call. = FALSE)  
  }
  # check that each element has been correctly defined
  for(cov in seq(along = covariate.list)){
    # Check the list is either length of 1 or equal to strata number
    if(length(covariate.list[[cov]]) != no.strata){
      if(length(covariate.list[[cov]]) == 1){
        # If only one element has been supplied then replicate it for all strata
        for(strat in 2:no.strata){
          covariate.list[[cov]][[strat]] <- covariate.list[[cov]][[1]]
        }
      }else{
        stop(paste("You have supplied an incorrect number of covariate distributions for cov ", list.names[cov], " the list should either be of length 1 or equal to the number of strata.", sep = ""), call. = FALSE)
      }
    }
    # For each strata
    for(strat in 1:no.strata){
      next.element <- covariate.list[[cov]][[strat]]
      if(class(next.element) == "data.frame"){
        # check it has 2 columns
        if(ncol(next.element) != 2){
          stop(paste("The data.frame for covariate ", list.names[cov]," and strata ", strat," should have 2 columns: level and prob.", sep = ""), call. = FALSE)
        }
        # check the column names are level and prob
        col.names <- names(next.element)
        if(!all(col.names %in% c("level", "prob"))){
          stop(paste("The data.frame for covariate ", list.names[cov]," and strata ", strat," should have 2 columns: level and prob.", sep = ""), call. = FALSE)  
        }
        # check prob column sums to 1
        if(sum(next.element$prob) != 1){
          stop(paste("The probabilities for covariate ", list.names[cov]," and strata ", strat," do not sum to 1.", sep = ""), call. = FALSE) 
        }
      }else if(class(next.element) == "list"){
        # Check distributions
        distribution <- next.element[[1]]
        # Check that the second element is a list with the correct parameters
        params <- next.element[[2]]
        param.names <- names(params)
        if(distribution == "normal"){
          if(length(param.names) != 2 | !all(param.names %in% c("mean", "sd"))){
            stop(paste("The distribution parameters for covariate ", list.names[cov]," and strata ", strat," should be mean and sd.", sep = ""), call. = FALSE)  
          }
        }else if(distribution  == "lognormal"){
          if(length(param.names) != 2 | !all(param.names %in% c("meanlog", "sdlog"))){
            stop(paste("The distribution parameters for covariate ", list.names[cov]," and strata ", strat," should be meanlog and sdlog.", sep = ""), call. = FALSE)  
          }
        }else if(distribution == "poisson"){
          if(length(param.names) != 1 | !all(param.names %in% c("lambda"))){
            stop(paste("The distribution parameter for covariate ", list.names[cov]," and strata ", strat," should be lambda.", sep = ""), call. = FALSE)  
          }
        }else if(distribution == "ztruncpois"){
          if(length(param.names) != 1 | !all(param.names %in% c("mean"))){
            stop(paste("The distribution parameter for covariate ", list.names[cov]," and strata ", strat," should be mean.", sep = ""), call. = FALSE)  
          }
        }else{
          stop(paste("The distribution for covariate ", list.names[cov]," and strata ", strat," is not implemented at present. Please select from: normal, lognormal, poisson and ztruncpois.", sep = ""), call. = FALSE)
        }
      }else{
        stop(paste("Element ", cov, " of your covariate list is not an accepted format. Please supply either a data.frame or a list", sep = ""), call. = FALSE)
      }
    }
  }
  return(covariate.list)
}