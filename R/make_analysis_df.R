#' Create analysis table
#'
#' Takes a collection of chapters, breaks
#' them up into individual chapters, and returns summary
#' statistics for each chapter. Output statistics include average sentence length,
#' average token length, average type length, average variety 
#' (# type/# token), story length, number of occurences of words, 
#' number of occurances of punctuation and average sentiment score
#' (AFINN method).
#'
#' @param text Character vector containing all the lines in a given text 
#' @param chapters Line index numbers associated with the first line/title 
#'        line of each chapter
#' @param freqwords Words whose frequency of appearance will be recorded
#' @param punctlist Punctuation characters whose frequency of appearance 
#'        will be recorded 
#' @export
#' @examples
#' breaks <- find_chapters(gardenParty)
#' words <- c("the", "and", "a")
#' punctuation <- c(",", ".", "...")
#' make_analysis_df(text = gardenParty ,chapters = breaks, 
#'                  freqWords = words, punctlist = punctuation)

make_analysis_df <- function(text, chapters, freqwords, punctlist){
  collection_name <- c()
  story_title <- c()
  outputdf <- NULL
  for(i in 1:(length(chapters) - 1)){
    col_name <- unlist(strsplit(text[chapters[i]], ","))[1]
    collection_name <- c(collection_name, col_name)
    start <- chapters[i]
    end <- chapters[i+1] - 1
    text.lines <- text[start:end]
    text.lines <- gsub("^([A-Z]+[a-z]+\\s[A-Z]+[a-z]+|[A-Z]+[a-z]+|[A-Z]+[a-z]+\\s[A-Z]+[a-z]+\\s[A-Z]+[a-z]+), and other stories, by Katherine Mansfield : ", "", text.lines)
    story_title <- c(story_title, text.lines[1])
    subtext <- collapse_text(text.lines)
    
    # average token length
    text.token <- extract_token(subtext)
    text.token.length <- word_length(text.token)
    text.token.averageLength <- mean(text.token.length)
    
    # average type length
    text.type <- extract_type(subtext)
    text.type.length <- word_length(text.type)
    text.type.averageLength <- mean(text.type.length)
    
    # average sentence length 
    text.sentences <- extract_sentences(subtext)
    text.sentences.length <- sentence_length(text.sentences)
    text.sentences.averageLength <- mean(text.sentences.length)
    
    # variety of words (type/token)
    text.variety <- get_variety(subtext)
    text.variety <- mean(text.variety)
    
    statnames <- c("avg_token_length", "avg_type_length", "variety", 
                   "avg_sentence_length", "story_length")
    wordstats <- c(text.token.averageLength, text.type.averageLength, text.variety, 
                   text.sentences.averageLength, length(text.token.length))
    worddf <- data.frame(statnames, wordstats)
    worddf <- tidyr::spread(worddf, statnames, wordstats)
    
    # frequency of most common words
    WordFreq <- charfreq(text.token, freqwords, punctuation = FALSE)
    WordFreq <- mutate(WordFreq, freq = freq/length(text.token))
    WordFreq <- tidyr::spread(WordFreq, character, freq)
    
    # punctuation
    text.punct <- extract_punct(subtext)
    PunctFreq <- charfreq(text.punct, punctlist, punctuation = TRUE)
    PunctFreq <- mutate(PunctFreq, freq = freq/length(text.punct))
    PunctFreq <- tidyr::spread(PunctFreq, character, freq)
    
    text.sentiment <- extract_sentences(subtext)
    sentiment <- syuzhet::get_sentiment(text.sentiment, method = "afinn")
    sentmean <- tail(sentiment, 100)%>%
      mean()
    
    chapterSummary <- cbind(worddf, WordFreq, PunctFreq, sentmean)
    if(i == 1){
      outputdf <- chapterSummary
    }
    else{
      outputdf <- rbind(outputdf, chapterSummary)
    }
  }
  
  outputdf <- cbind(collection_name, story_title, outputdf)
  return(outputdf)
}