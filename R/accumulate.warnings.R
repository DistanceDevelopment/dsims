accumulate.warnings <- function(warnings.list){
  #Formats the warnings coming back from a parallel processor run
  if(length(warnings.list) > 0){
    warnings <- list()
    for(i in seq(along = warnings.list)){
      message <- warnings.list[[i]]$message
      index <- warnings.list[[i]]$index
      count <- warnings.list[[i]]$counter
      for(j in seq(along = message)){
        warnings <- message.handler(warnings,
                                    message[[j]],
                                    i = index[[j]],
                                    count = count[[j]])
      }
    }
    return(warnings)
  }else{
    return(list())
  }
}
