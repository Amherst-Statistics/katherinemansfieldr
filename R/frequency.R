#' Counts frequency of character or word in a text
#'
#' This function matches a list of strings to a text and counts how 
#' many times a particular string occurs in the text. Returns a data
#' table with the matched strings with its number the occurrances.
#'
#' @param characters Words to be matched with a smaller list of desired 
#'        words
#' @param char.list Words whose frequency we are interested in measuring
#' @param punctuation Boolean that determines whether to convert
#'        punctuation marks into words
#' @export
#' @examples
#' sentence_length(sentences=c("And after all the weather was ideal.", 
#' "Windless, warm, the sky without a cloud."))

charfreq <- function(characters, char.list, punctuation = FALSE){
  freq <- c()
  char.list[which(char.list=="?")] <- "\\?"
  for(i in 1:length(char.list)){
    x <- length(grep(char.list[i], characters))
    freq <- c(freq, x)
    if(punctuation == TRUE){
      if (char.list[i] == ","){
        char.list[i] <- "comma"
      }
      if (char.list[i] == "—"){
        char.list[i] <- "em-dash"
      }
      if (char.list[i] == "."){
        char.list[i] <- "period"
      }
      if (char.list[i] == "\\?"){
        char.list[i] <- "question-mark"
      }
      if (char.list[i] == "!"){
        char.list[i] <- "exclaim-point"
      }
      if (char.list[i] == "..."){
        char.list[i] <- "ellipsis"
      }
      if (char.list[i] == ";"){
        char.list[i] <- "semicolon"
      }
      if (char.list[i] == "“" | char.list[i] == "”"){
        char.list[i] <- "quote"
      }
    }
    char.list[i] <- paste("char_", char.list[i], sep = "")
  }
  
  output <- data.frame(char.list, freq)
  colnames(output) <- c("char_type", "freq")
  return(output)
}

#' Gets frequency of words per line
#'
#' Returns a data table returning the frequency at which a series of desired
#' words appear in a given line and the index number of that line.
#'
#' @param text Vector of strings representing lines of a text
#' @param freqwords Words whose frequency we are interested in finding
#' @export
#' @examples
#' freqWord_line(text = gardenParty, freqwords = c("she", "they", "he"))

freqWord_line <- function(text, freqwords){
  line_index <- c()
  frequency <- NA
  for(i in 1:length(text)){
    words <- extract_token(text[i])
    WordsFreq <- charfreq(words, freqwords)
    line_index <- c(line_index, paste(i))
    WordsFreq <- mutate(WordsFreq, freq = freq/length(words))
    WordsFreq <- spread(WordsFreq, char_type, freq)
    if(i == 1){
      frequency <- WordsFreq
    }
    else{
      frequency <- rbind(frequency, WordsFreq)
    }
  }
  line_index <- as.numeric(line_index)
  output <- cbind(line_index, frequency)
  return(output)
}

#' Gets frequency of punctuation per line
#'
#' Returns a data table returning the frequency at which a series of 
#' desired punctuation marks appear in a given line and the index 
#' number of that line.
#'
#' @param text Vector of strings representing lines of a text
#' @param punctlist Punctuation marks whose frequency we are 
#'        interested in finding
#' @export
#' @examples
#' freqPunct_line(text = gardenParty, punctlist = c(".", "?", "..."))

freqPunct_line <- function(text, punctlist){
  line_index <- c()
  frequency <- NA
  for(i in 1:length(text)){
    words <- extract_punct(text[i])
    WordsFreq <- charfreq(words, punctlist, punctuation = TRUE)
    line_index <- c(line_index, paste(i))
    WordsFreq <- mutate(WordsFreq, freq = freq)
    WordsFreq <- spread(WordsFreq, char_type, freq)
    if(i == 1){
      frequency <- WordsFreq
    }
    else{
      frequency <- rbind(frequency, WordsFreq)
    }
  }
  line_index <- as.numeric(line_index)
  output <- cbind(line_index, frequency)
  return(output)
}
