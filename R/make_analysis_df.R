#' Create analysis table
#'
#' This function takes a collection of chapters, breaks
#' them up into individual chapters, and returns summary
#' statistics for each chapter. Outputs average sentence length,
#' average token length, average type length, average variety 
#' (# type/# token), story length, number of occurences of words, 
#' number of occurances of punctuation and average sentiment score
#' (AFINN method).
#'
#' @param text Character vector containing all the lines in a given text 
#' @param chapters Lines to be matched in the text
#' @param freqwords Words whose frequency of appearance will be recorded
#' @param punctlist Punctuation characters whose frequency of appearance 
#'        will be recorded 
#' @export
#' @examples
#' make_analysis_df(text="gardenParty",chapters=chapter.breaks, 
#'                  freqWords=c("the", "and", "a"))

make_analysis_df <- function(text, chapters, freqwords, punctlist){
  collection_name <- unlist(strsplit(text[1]))
  collection_name <- rep(collection_name, length(chapters) - 1)
  story_title <- c()
  outputdf <- NULL
  for(i in 1:(length(chapters) - 1)){
    start <- chapters[i]
    end <- chapters[i+1] - 1
    text.lines <- text[start:end]
    story_title <- c(story_title, text.lines[1])
    subtext <- collapseText(text.lines)
    
    # average token length
    text.token <- extractToken(subtext)
    text.token.length <- getWordLength(story_title, text.token)
    text.token.averageLength <- mean(text.token.length$word_length)
    
    # average type length
    text.type <- extractType(subtext)
    text.type.length <- getWordLength(story_title, text.type)
    text.type.averageLength <- mean(text.type.length$word_length)
    
    # average sentence length 
    text.sentences <- extractSentences(subtext)
    text.sentences.length <- getSentenceLength("part1", text.sentences)
    text.sentences.averageLength <- mean(text.sentences.length$sentence_length)
    
    # variety of words (type/token)
    text.variety <- getVariety(subtext)
    text.variety <- mean(text.variety$variety)
    
    statnames <- c("avg_token_length", "avg_type_length", "variety", "avg_sentence_length", "story_length")
    wordstats <- c(text.token.averageLength, text.type.averageLength, text.variety, text.sentences.averageLength, length(text.token.length[,2]))
    worddf <- data.frame(statnames, wordstats)
    worddf <- spread(worddf, statnames, wordstats)
    
    # frequency of most common words
    WordFreq <- getCharFreq("part1", text.token, freqWords)
    WordFreq <- mutate(WordFreq, freq = freq/length(text.token))
    WordFreq <- spread(WordFreq, char_type, freq)
    
    # punctuation
    text.punct <- extractPunctuation(subtext)
    PunctFreq <- getCharFreq("part1", text.punct, punctList)
    PunctFreq <- mutate(PunctFreq, freq = freq/length(text.punct))
    PunctFreq <- spread(PunctFreq, char_type, freq)
    
    chapterSummary <- cbind(worddf, WordFreq, PunctFreq)
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