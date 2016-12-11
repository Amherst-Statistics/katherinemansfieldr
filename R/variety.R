
#' Gets variety value
#'
#' Returns the ratio of number of types by number of tokens in a given 
#' string of words.
#'
#' @param text String or vector of strings
#' @export
#' @examples
#' get_variety(text=gardenParty)

get_variety <- function(text){
  line_index <- c()
  variety <- c()
  for(i in 1:length(text)){
    token <- extractToken(text[i])
    type <- extractType(text[i])
    sentences <- extractSentences(text[i])
    var <- length(type)/length(token)
    line_index <- c(line_index, paste(i))
    variety <- c(variety, var)
  }
  line_index <- as.numeric(line_index)
  output <- data.frame(line_index, variety)
  return(output)
}