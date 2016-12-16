#' Count frequency of word or punctuation mark in a text
#'
#' Matches a vector of target words/punctuation marks to a larger vector 
#' of words/punctuation marks and counts how many times that particular word or 
#' punctuation marks occurs in the larger string. Returns a data table with the 
#' matched words or punctuation marks with their number the occurrances 
#' in the larger string.
#'
#' @param characters Large vector of words/punctuation marks to be matched with 
#'        a smaller list of desired words/characters.
#' @param char.list Vector of target words/punctuation marks to be matched with the larger
#'        vector of words/characters
#' @param punctuation Boolean that determines whether to convert
#'        punctuation marks into words
#' @note the accepted punctuation marks are commas, periods, semicolons, question marks
#'        exclamation points, quotation marks (forward and backward), ellipses and em-dashes.
#' @export
#' @examples
#' char <- extract_token(gardenParty)
#' charfreq(char, c("she", "he", "them"), punctuation = FALSE)
#'      character freq
#' [1]       she  1349
#' [2]        he  8632
#' [3]      them  223
#' 
#' char <- extract_punct(gardenParty)
#' charfreq(char, c(".", "...", "?"), punctuation = TRUE)
#'           character    freq
#' [1]          period   15229
#' [2]        ellipsis     199
#' [3]   question_mark     603

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
        char.list[i] <- "em_dash"
      }
      if (char.list[i] == "."){
        char.list[i] <- "period"
      }
      if (char.list[i] == "\\?"){
        char.list[i] <- "question_mark"
      }
      if (char.list[i] == "!"){
        char.list[i] <- "exclaim_point"
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
  }
  
  output <- data.frame(char.list, freq)
  colnames(output) <- c("character", "freq")
  return(output)
}

#' Get frequency of words per line
#'
#' Returns a data table returning the relative frequency at which a series of desired
#' words appear in a given line of text and the index number of that line. Relative
#' frequency is calculated as the ratio of number of occurances to number of total
#' words in a given line.
#'
#' @param text Any text or document as a character vector
#' @param freqwords Vector of target words 
#'        to be matched with the text/document of interest
#' @export
#' @examples
#' freq_word_line(text = gardenParty, freqwords = c("she", "they", "he"))

freq_word_line <- function(text, freqwords){
  line_index <- c()
  frequency <- NA
  for(i in 1:length(text)){
    words <- extract_token(text[i])
    WordsFreq <- charfreq(words, freqwords)
    line_index <- c(line_index, paste(i))
    WordsFreq <- dplyr::mutate(WordsFreq, freq = freq/length(words))
    WordsFreq <- tidyr::spread(WordsFreq, character, freq)
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

#' Get frequency of punctuation marks per line
#'
#' Returns a data table returning the relative frequency at which a series of 
#' desired punctuation marks appear in a given line and the index 
#' number of that line. Relative frequency is calculated as the ratio of number
#' of occurances to total number of words in a given line.
#'
#' @param text Vector of strings representing lines of a text
#' @param punctlist Vector of target punctuation marks to be matched 
#'        with the text/document of interest
#' @export
#' @examples
#' freqPunct_line(text = gardenParty, punctlist = c(".", "?", "..."))

freq_punct_line <- function(text, punctlist){
  line_index <- c()
  frequency <- NA
  for(i in 1:length(text)){
    words <- extract_punct(text[i])
    WordsFreq <- charfreq(words, punctlist, punctuation = TRUE)
    line_index <- c(line_index, paste(i))
    WordsFreq <- dplyr::mutate(WordsFreq, freq = freq/length(extract_token(text[i])))
    WordsFreq <- tidyr::spread(WordsFreq, character, freq)
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
