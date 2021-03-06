% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_analysis_df.R
\name{make_analysis_df}
\alias{make_analysis_df}
\title{Create analysis table}
\usage{
make_analysis_df(text, book, chapters, titles, freqwords, punctlist)
}
\arguments{
\item{text}{Character vector containing all the lines in a given text}

\item{book}{Name of book or story collection from which chapters/stories are taken}

\item{chapters}{Line index numbers associated with the first line/title 
line of each chapter}

\item{titles}{Title of each story or chapter in output dataset}

\item{freqwords}{Words whose frequency of appearance will be recorded}

\item{punctlist}{Punctuation characters whose frequency of appearance 
will be recorded}
}
\description{
Takes a collection of chapters, breaks
them up into individual chapters, and returns summary dataset
for each chapter. Output statistics include average sentence length,
average token length, average type length, average variety 
(# type/# token), story length (number of words), number of occurences of words (expressed as
ratio of frequency of word over total number of words), 
number of occurances of punctuation (expressed as ratio of frequency of
punctuation mark over all punctuation marks) and average sentiment score
(AFINN method).
}
\examples{
sample <- gardenParty[1:452]
collection <- "The Garden Party"
breaks <- find_chapters(sample, " and other stories, by Katherine Mansfield : ")
stories <- c("At the Bay", "Her First Ball", "The Singing Lesson")
words <- c("the", "and", "a")
punctuation <- c(",", ".", "...")
make_analysis_df(text = sample, book = collection, chapters = breaks, titles = stories,
                 freqwords = words, punctlist = punctuation)
}

