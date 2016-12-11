#' Extract tokens
#'
#' Takes a string and returns a vector
#' of every word in the string.
#'
#' @param text Random string
#' @export
#' @examples
#' extract_token(text=gardenParty)

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
#' of each unique word in the string.
#'
#' @param text Random string
#' @export
#' @examples
#' extract_type(text=gardenParty)

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
#' @param text Random string
#' @export
#' @examples
#' extract_sentence(text="gardenParty")

extract_sentence <- function(text){
  output <- gsub("(”|“)", "", text)
  output <- strsplit(output, split = "[.?!] ") %>%
    unlist()
  output <- output[which(output != "")]
  return(output)
}

#' extract punctuation
#'
#' given a string and a list of punc
#'
#' @param text character vector
#' @param breaks vector of strings 
#' @export
#' @examples
#' extract_sentence(text=gardenParty)

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
