#' Extract tokens
#'
#' Takes a string and returns a vector
#' with every word in the string as a separate
#' element.
#'
#' @param text A random string, as a character vector
#' @export
#' @examples
#' sampleText <- gardenParty[10]
#' extract_token(text = sampleText)
#' [1] ... "why" "the" "dickens" "didn't" "the" ...

extract_token <- function(text){
  output <- tolower(text)
  output <- gsub("[^[:alnum:][:space:]’]", "", output) %>%
    strsplit("\\s") %>%
    unlist()
  output <- output[which(output != "")]
  return(output)
}

#' Extract types
#'
#' Takes a string and returns a vector
#' with every unique word in the string as
#' a separate element.
#'
#' @param text A random string, as a character vector
#' @export
#' @examples
#' sampleText <- gardenParty[10]
#' extract_type(text = sampleText)
#' [1]... "the" "dickens" "didn't" "fellow" "stick" ...

extract_type <- function(text){
  output <- tolower(text)
  output <- gsub("[^[:alnum:][:space:]’]", "", output) %>%
    strsplit("\\s") %>%
    unlist()
  output <- unique(output[which(output != "")])
  return(output)
}

#' Extract sentences
#'
#' Takes a string and returns a vector
#' of each sentence in the string.
#'
#' @param text A random string, as a character vector
#' @note This function takes out all periods, question marks, 
#'       exclamation points and quotation marks.
#' @export
#' @examples
#' sampleText <- gardenParty[10]
#' extract_sentences(text = sampleText)
#' [1] "Yes, very fine said Stanley briefly"
#' [2] "Why the dickens didn't the fellow stick to his part of the sea"
#' [3] "Why should he come barging over to this exact spot"

extract_sentences <- function(text){
  output <- gsub("([?!]”|“ | ”)", "", text)
  output <- strsplit(output, split = "[.?!] ") %>%
    unlist()
  output <- output[which(output != "")]
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
#' @param text A random string, as a character vector
#' @export
#' @examples
#' sampleText <- gardenParty[10]
#' extract_punct(text = sampleText)
#' [1] "“" "," "." "’" "?" "?" ....

extract_punct <- function(text){
  p <- as.vector(tolower(text))
  p <- strsplit(p, "( [a-z]+|[a-z]+)") %>%
    unlist()
  p <- gsub(". . .", "\\.\\.\\.", p)  
  ellipsis <- grep("\\.\\.\\.", p, value = TRUE) %>%
    strsplit(" ") %>% unlist()
  addons <- ellipsis[which(ellipsis != "...")]
  ellipsis <- ellipsis[which(ellipsis == "...")]
  for(i in 1:length(p)){
    if(nchar(p[i]) > 1){
      x <- strsplit(p[i], "") %>% unlist()
      addons <- c(addons, x)
      p[i] <- " "
    }
  }
  output <- c(p, ellipsis, addons)
  output <- grep("[.,?;!”“—\\.\\.\\.]", output, value = TRUE)
  return(output)
}
