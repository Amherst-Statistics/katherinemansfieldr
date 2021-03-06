#' Get type-token ratio/vocabulary richness measure
#'
#' Returns the ratio of number of types by number of tokens in a given 
#' string.
#'
#' @param text A vector of strings containing lines from a text.
#' @export
#' @examples
#' get_variety(text=gardenParty)

get_variety <- function(text){
  variety <- c()
  for(i in 1:length(text)){
    token <- extract_token(text[i])
    type <- extract_type(text[i])
    var <- length(type)/length(token)
    variety <- c(variety, var)
  }
  output <- variety
  return(output)
}