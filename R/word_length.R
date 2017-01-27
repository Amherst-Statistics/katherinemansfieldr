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

word_length <- function(words){
  output <- nchar(words)
  return(output)
}

#' Get token length per line
#'
#' Returns a vector containing the average token (word) length in a line of text
#' in terms of number of characters.
#'
#' @param text Vector of strings, with each element representing one line from a text
#' @export
#' @examples
#' word_length_line(text = gardenParty)

word_length_line <- function(text){
  line_index <- c()
  avg_token_length <- c()
  for(i in 1:length(text)){
    token <- extract_token(text[i])
    tokenLength <- word_length(token)
    tokenLengthAvg <- mean(tokenLength)
    line_index <- c(line_index, paste(i))
    avg_token_length <- c(avg_token_length, tokenLengthAvg)
  }
  line_index <- as.numeric(line_index)
  output <- data.frame(line_index, avg_token_length)
  return(output)
}
