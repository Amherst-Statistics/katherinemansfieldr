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
#' [1] 1215 1535

get_breaks <- function(text, breaks){
  output <- c()
  for(i in 1:(length(breaks) + 1)){
    output <- c(output, which(gardenParty.text == breakpoints[i]))
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
#' [1] "“Oh, Mrs. Parker, I’m going out.” “Very good, sir.”"

collapse_text <- function(text){
  return(paste(text, collapse = " "))
}

#' Find 10 most frequent words
#'
#' Retrieves the 10 most frequently occurring
#' words and returns the words in a vector.
#'
#' @param text Character vector containing all the lines in a given text
#' @export
#' @examples
#' find_freq_char(text = gardenParty)
#' [1] "the" "and" "a" "to" "she" "was" "her" "it" "of" "in"

find_freq_char <- function(text){
  freqWords <- collapse_text(text) %>%
    extract_token()
  
  words <- table(freqWords) %>%
    sort(decreasing = TRUE) %>%
    head(10) %>%
    data.frame()
  
  freqWords <- as.character(words$freqWords)
  
  return(freqWords)
}

#' Find chapter breaks in Mansfield data
#'
#' Returns the line index numbers of the first lines of each short story in
#' the vectorized short story collections.
#'
#' @param text Character vector containing all the lines in a given text 
#' @note The last element in the output is the line 
#' @export
#' @examples
#' find_chapters(text = gardenParty)
#' [1] 1  352  403 ...

find_chapters <- function(text){
  output <- grep("^([A-Z]+[a-z]+\\s[A-Z]+[a-z]+|[A-Z]+[a-z]+|[A-Z]+[a-z]+\\s[A-Z]+[a-z]+\\s[A-Z]+[a-z]+), and other stories, by Katherine Mansfield : ", text)
  return(output)
}
