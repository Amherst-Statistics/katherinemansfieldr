#' Break text by line matching
#'
#' This function matches a vector of break point strings 
#' with lines from a larger text and returns 
#' the line/index numbers of the matched strings in the larger text.
#'
#' @param text Character vector containing all the lines in a given text 
#' @param breaks Lines to be matched in the text
#' @note The breaks argument can be fragments of the lines that are to be
#' matched and indexed in the larger text argument.
#' @export
#' @examples
#' breakpoints <- c("Shall we sit here?", "“No, not now,” 
#'                said the girl. “Not here, I can’t.”")
#' get_breaks(text = gardenParty, breaks = breakpoints)

get_breaks <- function(text, breaks){
  output <- c()
  for(i in 1:length(breaks)){
    output <- c(output, grep(breaks[i], text))
  }
  return(output)
}

#' Collapse text
#'
#' Retrieves a character vector and collapses it into one string.
#'
#' @param text Character vector containing all the lines in a given text
#' @export
#' @examples
#' sampleText <- c("“Oh, Mrs. Parker, I’m going out.”", "“Very good, sir.”")
#' collapse_text(text = sampleText)

collapse_text <- function(text){
  return(paste(text, collapse = " "))
}

#' Find most frequent words
#'
#' Retrieves the n most frequently occurring
#' words and returns the words in a vector.
#'
#' @param text Character vector containing all the lines in a given text
#' @param numwords Number of most frequently occuring words to retrieve
#' @importFrom dplyr %>%
#' @importFrom utils head
#' @export
#' @examples
#' find_freq_char(text = gardenParty, 10)

find_freq_char <- function(text, numwords){
  freqWords <- collapse_text(text) %>%
    extract_token()
  
  words <- table(freqWords) %>%
    sort(decreasing = TRUE) %>%
    head(numwords) %>%
    names()
  
  return(words)
}

#' Find chapter breaks in a text
#'
#' Returns the line index numbers of the titles of each short story or chapter in
#' the a vectorized short story collection or book. 
#'
#' @param text Character vector containing all the lines in a given text 
#' @param keyword Character string of keywords that identify the title lines of each chapter.
#' @note The last element in the output is the line 
#' @export
#' @examples
#' chapters <- c(" and other stories, by Katherine Mansfield : ")
#' find_chapters(text = gardenParty, keyword = chapters)

find_chapters <- function(text, keyword){
  output <- grep(keyword, text)
  output <- as.integer(output)
  return(output)
}