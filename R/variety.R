#' Get type-token ratio/vocabulary richness measure
#'
#' Returns the ratio of number of types by number of tokens in a given 
#' character vector. Each element in the output vector is associated with 
#' the line with the same index number in the input vector.
#'
#' @param text Vector of strings representing lines of a text
#' @export
#' @examples
#' get_variety(text=gardenParty)
#' [1] 0.916  0.6071  0.7536  ...

get_variety <- function(text){
  variety <- c()
  for(i in 1:length(text)){
    token <- extract_token(text[i])
    type <- extract_type(text[i])
    sentences <- extract_sentence(text[i])
    var <- length(type)/length(token)
    variety <- c(variety, var)
  }
  output <- variety
  return(output)
}