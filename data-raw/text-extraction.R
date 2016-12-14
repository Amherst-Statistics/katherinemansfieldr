## convert HTML into text files and character vectors
## and add them to the package

library(mosaic) 
library(XML)
library(readr)
library(devtools)
library(stringr)
library(RCurl)

createURL <- function(nums, site){
  x <- c()
    for(i in 1:length(nums)){
      y <- paste(site, nums[i], ".html", sep = "")
      x <-c(x, y)
    }
  return(x)
}

# Function that converts HTML to text file for each story
createTextfile <- function(site){
  name <- deparse(substitute(site))
  for(i in 1:length(site)){
    html <- getURL(site[i], followlocation = TRUE)
    
    # parse html
    doc = htmlParse(html, encoding = "UTF-8", asText=TRUE)
    title <- xpathSApply(doc, "//title", xmlValue)
    plain.text <- xpathSApply(doc, "//p", xmlValue)
    plain.text <- c(title, plain.text)
    cat(paste(plain.text, collapse = "\n"), file = paste(name, ".", i, ".txt", sep = ""), sep = "")
  }
}

scanMultiple <- function(list){
  output <- c()
  for(i in 1:length(list)){
    story <- scan(list[i], what = "character", sep = "\n")
    story.text <- story[1:(length(story) - 3)]
    output <- c(output, story.text)
  }
  return(output)
}

# Read and parse HTML file
somethingChildishURL <- "https://ebooks.adelaide.edu.au/m/mansfield/katherine/something/chapter"
blissURL <- "https://ebooks.adelaide.edu.au/m/mansfield/katherine/bliss/chapter"
gardenPartyURL <- "https://ebooks.adelaide.edu.au/m/mansfield/katherine/garden/chapter"

somethingChildishChapterNum <- as.character(seq(1:25))
blissChapterNum <- as.character(seq(1:14))
gardenPartyChapterNum <- as.character(seq(1:15))

# create URLs for all stories
bliss <- createURL(blissChapterNum, blissURL)
somethingChildish <- createURL(somethingChildishChapterNum, somethingChildishURL)
gardenParty <- createURL(gardenPartyChapterNum, gardenPartyURL)

# store text files into separate folder
createTextfile(bliss)
createTextfile(somethingChildish)
createTextfile(gardenParty)

bliss.list <- list.files(pattern = "bliss\\..+\\.txt")
gardenParty.list <- list.files(pattern = "gardenParty\\..+\\.txt")
somethingChildish.list <- list.files(pattern = "somethingChildish\\..+\\.txt")

bliss <- scanMultiple(bliss.list)
gardenParty <- scanMultiple(gardenParty.list)
somethingChildish <- scanMultiple(somethingChildish.list)
mansfieldComplete <- c(somethingChildish, bliss, gardenParty)

devtools::use_data(mansfieldComplete, overwrite = TRUE)
devtools::use_data(bliss, overwrite = TRUE)
devtools::use_data(gardenParty, overwrite = TRUE)
devtools::use_data(somethingChildish, overwrite = TRUE)