#' Gets word length
#'
#' Returns the length of a word in terms of number of
#' unique characters
#'
#' @param words Vector of words
#' @export
#' @examples
#' word_length(words=c("I", "am", "Sam"))

word_length <- function(words){
  word_length <- nchar(words)
  word_index <- c()
  for(i in 1:length(words)){
    word_index <- c(word_index, paste(i))
  }
  word_index <- as.numeric(word_index)
  output <- data.frame(word_length, word_index)
  return(output)
}

#' Gets word length per line
#'
#' Returns a data table returning the average word length in a line
#' in terms of number of characters and the index number of that line.
#'
#' @param text Vector of strings representing lines of a text
#' @export
#' @examples
#' word_length_line(text = "gardenParty)

word_length_line <- function(text){
  line_index <- c()
  avg_token_length <- c()
  for(i in 1:length(text)){
    token <- extract_token(text[i])
    tokenLength <- word_length(token)
    tokenLengthAvg <- mean(tokenLength$word_length)
    line_index <- c(line_index, paste(i))
    avg_token_length <- c(avg_token_length, tokenLengthAvg)
  }
  line_index <- as.numeric(line_index)
  output <- data.frame(line_index, avg_token_length)
  return(output)
}
