#' Get token length
#'
#' Retrieves a vector of tokens (words) and returns a vector 
#' containing the length of each token in terms of number of
#' unique characters.
#'
#' @param words A vector of words
#' @export
#' @examples
#' word_length(words=c("very", "early", "morning"))
#' [1] 4, 5, 7

word_length <- function(words){
  output <- nchar(words)
  return(output)
}

#' Get token length per line
#'
#' Returns a vector containing the average token (word) length in a line
#' in terms of number of characters.
#'
#' @param text Vector of strings representing lines of a text
#' @export
#' @examples
#' word_length_line(text = gardenParty)
#'   avg_token_length
#' 1            4.750
#' 2            4.214
#' 3            4.579

word_length_line <- function(text){
  avg_token_length <- c()
  for(i in 1:length(text)){
    token <- extract_token(text[i])
    tokenLength <- word_length(token)
    tokenLengthAvg <- mean(tokenLength)
    avg_token_length <- c(avg_token_length, tokenLengthAvg)
  }
  output <- avg_token_length
  return(output)
}
