#' Get sentence length
#'
#' Retrieves a vector of sentences and returns the length of 
#' each sentence in terms of number of words.
#'
#' @param sentences A character vector of sentences
#' @export
#' @examples
#' sentence_length(sentences=c("And after all the weather was ideal.", 
#' "Windless, warm, the sky without a cloud."))

sentence_length <- function(sentences){
  output <- sapply(gregexpr("\\W+", sentences), length)
  output <- as.numeric(output)
  output <- output + 1
  return(output)
}

#' Get sentence length per line
#'
#' Returns a data table returning the average sentence length in a line
#' in terms of number of words and the index number of that line.
#'
#' @param text Vector of strings representing lines of a text
#' @export
#' @examples
#' sentence_length_line(text=gardenParty)

sentence_length_line <- function(text){
  line_index <- c()
  avg_sentence_length <- c()
  for(i in 1:length(text)){
    sentence <- extract_sentences(text[i])
    sentenceLength <- sentence_length(sentence)
    sentenceLengthAvg <- mean(sentenceLength)
    line_index <- c(line_index, paste(i))
    avg_sentence_length <- c(avg_sentence_length, sentenceLengthAvg)
  }
  line_index <- as.numeric(line_index)
  output <- data.frame(line_index, avg_sentence_length)
  return(output)
}
