#' Extract tokens
#'
#' Takes a string and returns a vector
#' with every word in the string as a separate
#' element.
#'
#' @param text Character vector containing all the lines in a given text 
#' @importFrom dplyr %>%
#' @export
#' @examples
#' sampleText <- gardenParty[10]
#' extract_token(text = sampleText)

extract_token <- function(text){
  output <- tolower(text)
  output <- gsub("[.,?;:!\u201C\u201D\u2014\\.\\.\\.]", " ", output) %>%
    strsplit("\\s") %>%
    unlist()
  output <- gsub("’", "'", output)
  output <- output[which(output != "")]
  return(output)
}

#' Extract types
#'
#' Takes a string and returns a vector
#' with every unique word in the string as
#' a separate element.
#'
#' @param text Character vector containing all the lines in a given text 
#' @importFrom dplyr %>%
#' @export
#' @examples
#' sampleText <- gardenParty[10]
#' extract_type(text = sampleText)

extract_type <- function(text){
  output <- tolower(text)
  output <- gsub("[.,?;:!\u201C\u201D\u2014\\.\\.\\.]", " ", output) %>%
    strsplit("\\s") %>%
    unlist()
  output <- gsub("’", "'", output)
  output <- unique(output[which(output != "")])
  return(output)
}

#' Extract sentences
#'
#' Takes a string and returns a vector
#' of each sentence in the string.
#'
#' @param text Character vector containing all the lines in a given text 
#' @note This function takes out all periods, question marks, 
#'       exclamation points and quotation marks.
#' @importFrom dplyr %>%
#' @export
#' @examples
#' sampleText <- gardenParty[10]
#' extract_sentences(text = sampleText)

extract_sentences <- function(text){
  output <- strsplit(text, split = "(?<=[.!?] |[[.!?]\u201D )", perl = TRUE) %>%
    unlist()
  output <- gsub("^\\s+|\\s+$", "", output)
  for(i in 1:length(output)){
    if(grepl("^([A-Z]|\u201C)", output[i], perl = TRUE) != TRUE){
      replace <- paste(output[i-1], output[i])
      output[i-1] <- replace
      output[i] <- ""
    }
  }
  output <- output[which(output != "")]
  output <- output[which(output != "...")]
  return(output)
}

#' extract punctuation
#'
#' Takes a string and returns a vector of every
#' punctuation mark present in the string. The punctuation
#' marks returned are: periods, commas, semicolons, question 
#' marks, exclamation points, quotation marks (both forward
#' and backward), ellipses and em-dashes.
#'
#' @param text Character vector containing all the lines in a given text 
#' @export
#' @importFrom dplyr %>%
#' @examples
#' sampleText <- gardenParty[10]
#' extract_punct(text = sampleText)

extract_punct <- function(text){
  output <- as.vector(tolower(text))
  output <- strsplit(output, "( [a-z]+|[a-z]+)") %>%
    unlist()
  output <- output[which(output != "")]
  output <- strsplit(output, " ") %>%
    unlist()
  output <- gsub("\u201D", "~\u201D", output)
  output <- strsplit(output, "~", perl = TRUE) %>%
    unlist()
  return(output)
}
