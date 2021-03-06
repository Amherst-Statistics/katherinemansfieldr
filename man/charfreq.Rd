% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/frequency.R
\name{charfreq}
\alias{charfreq}
\title{Count frequency of a word or punctuation mark in a text}
\usage{
charfreq(characters, char.list, punctuation = FALSE)
}
\arguments{
\item{characters}{Large vector of words/punctuation marks to be matched with 
a smaller list of desired words/characters.}

\item{char.list}{Vector of target words/punctuation marks to be matched with the larger
vector of words/characters}

\item{punctuation}{Boolean that determines whether to convert
punctuation marks into words in final output table}
}
\description{
Matches a vector of target words/punctuation marks to a larger vector 
of words/punctuation marks and counts how many times that particular word or 
punctuation marks occurs in the larger string. Returns a data table with the 
matched words or punctuation marks with their number the occurrances 
in the larger string.
}
\note{
the accepted punctuation marks are commas, periods, semicolons, question marks
       exclamation points, quotation marks (forward and backward), ellipses and em-dashes.

to match quotation marks, use Unicode characters for right (u201D) and left (u201C) 
      quotation marks.
}
\examples{
char <- extract_token(gardenParty)
charfreq(char, c("she", "he", "them"), punctuation = FALSE)

char <- extract_punct(gardenParty)
charfreq(char, c(".", "...", "?"), punctuation = TRUE)
}

