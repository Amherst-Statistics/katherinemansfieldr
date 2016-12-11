#' Break text by line matching
#'
#' This function matches a vector of break point strings 
#' with lines from another text and returns 
#' the line/index numbers of the matched strings in the text.
#'
#' @param text Character vector containing all the lines in a given text 
#' @param breaks Lines to be matched in the text
#' @export
#' @examples
#' get_breaks(text="gardenParty",breaks="breakpoints")

get_breaks <- function(text, breaks){
  output <- c()
  for(i in 1:length(breaks)){
    output <- c(output, which(text == breakpoints[i]))
  }
  return(output)
}

#' Collapse text
#'
#' This function retrieves a vector of character string objects 
#' and collapses it into one string.
#'
#' @param text Character vector containing all the lines in a given text
#' @export
#' @examples
#' collapse(text = "gardenParty")

collapse_text <- function(text){
  return(paste(text, collapse = " "))
}

#' Finds 10 most frequent words
#'
#' This function retrieves the 10 most frequently occurring
#' words and returns the words in a vector.
#'
#' @param text Character vector containing all the lines in a given text
#' @export
#' @examples
#' find_freq_char(text = "gardenParty")

find_freq_char <- function(text){
  freqWords <- collapseText(text) %>%
    extractToken()
  
  words <- table(freqWords) %>%
    sort(decreasing = TRUE) %>%
    head(10) %>%
    data.frame()
  
  freqWords <- as.character(words$freqWords)
  
  return(freqWords)
}

#' Scan multiple text files into character vector
#'
#' This function retrieves the names of a series of text files and
#' returns a vector composed of the lines of all the listed 
#' files.
#'
#' @param text Character vector containing all the lines in a given text
#' @param directory Local directory from which text files will be scanned
#' @export
#' @examples
#' find_freq_char(list=c("chapter1.txt", "chapter2.txt"), directory="~/STAT495-Group6/textFiles")

multi_scan <- function(list, directory){
  output <- c()
  for(i in 1:length(list)){
    story <- scan(list[i], what = "character", sep = "\n")
    story.text <- story[1:(length(story) - 3)]
    output <- c(output, story.text)
  }
  return(output)
}
