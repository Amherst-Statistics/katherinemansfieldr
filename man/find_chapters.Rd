% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/text_prep.R
\name{find_chapters}
\alias{find_chapters}
\title{Find chapter breaks in a text}
\usage{
find_chapters(text, keyword)
}
\arguments{
\item{text}{Character vector containing all the lines in a given text}

\item{keyword}{Character string of keywords that identify the title lines of each chapter.}
}
\description{
Returns the line index numbers of the titles of each short story or chapter in
the a vectorized short story collection or book.
}
\note{
The last element in the output is the line
}
\examples{
chapters <- c(" and other stories, by Katherine Mansfield : ")
find_chapters(text = gardenParty, keyword = chapters)
}

