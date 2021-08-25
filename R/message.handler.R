message.handler <- function(warnings, message=character(0), i, count = 1){
  # This function either stores or displays a summary of warnings
  # and errors.
  # ARGUMENTS
  #   - warnings is a list of two elements (message and counter)
  #   - message is the new message if one exists
  if(length(message) > 0){
    # Check if the warning already exists
    message <- paste(message, collapse = " ")
    index <- which(warnings$message == message)
    if(length(index) == 0){
      no.warnings <- length(warnings$message)
      warnings$message[[no.warnings + 1]] <- message
      warnings$counter[[no.warnings + 1]] <- count
      warnings$index[[no.warnings + 1]] <- i
    }else{
      warnings$counter[[index]] <- warnings$counter[[index]] + count
      warnings$index[[index]] <- c(warnings$index[[index]], i)
    }
  }
  return(warnings)
}

