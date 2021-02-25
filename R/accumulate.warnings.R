accumulate.warnings <- function(warnings.list){
  #Formats the warnings coming back from a parallel processor run
  if(length(warnings.list) > 0){
    warnings <- warnings.list[[1]]
    for(i in seq(along = warnings.list[-1])){
      temp <- warnings.list[[i]]$message
      for(j in seq(along = temp)){
        warnings <- message.handler(warnings, temp[[j]])
      }
    }
    return(warnings)
  }else{
    return(list())
  }
}
