scanMultiple <- function(list){
  output <- c()
  for(i in 1:length(list)){
    story <- scan(list[i], what = "character", sep = "\n")
    output <- c(output, story)
  }
  return(output)
}

setwd("~/git/katherinemansfieldr/data-raw")
bliss.list <- list.files(pattern = "bliss\\..+\\.txt")
gardenParty.list <- list.files(pattern = "gardenParty\\..+\\.txt")
somethingChildish.list <- list.files(pattern = "somethingChildish\\..+\\.txt")

# create the collection texts
bliss <- scanMultiple(bliss.list)
bliss <- gsub("“", "\u201C", bliss) 
bliss <- gsub("”", "\u201D", bliss)
bliss <- gsub("‘", "\u2018", bliss)
bliss <- gsub("’", "\u2019", bliss) 
bliss <- gsub("—", "\u2014", bliss) 
bliss <- gsub("–", "\u2014", bliss)

gardenParty <- scanMultiple(gardenParty.list)
gardenParty <- gsub("“", "\u201C", gardenParty) 
gardenParty <- gsub("”", "\u201D", gardenParty)
gardenParty <- gsub("‘", "\u2018", gardenParty)
gardenParty <- gsub("’", "\u2019", gardenParty) 
gardenParty <- gsub("—", "\u2014", gardenParty) 
gardenParty <- gsub("–", "\u2014", gardenParty) 

somethingChildish <- scanMultiple(somethingChildish.list)
somethingChildish <- gsub("“", "\u201C", somethingChildish) 
somethingChildish <- gsub("”", "\u201D", somethingChildish)
somethingChildish <- gsub("‘", "\u2018", somethingChildish)
somethingChildish <- gsub("’", "\u2019", somethingChildish) 
somethingChildish <- gsub("—", "\u2014", somethingChildish) 
somethingChildish <- gsub("–", "\u2014", somethingChildish) 

mansfieldComplete <- c(bliss, gardenParty, somethingChildish)

setwd("~/git/katherinemansfieldr/data")
save(bliss, file = "bliss.rda", compress = "bzip2")
save(gardenParty, file = "gardenParty.rda", compress = "bzip2")
save(somethingChildish, file = "somethingChildish.rda", compress = "bzip2")
save(mansfieldComplete, file = "mansfieldComplete.rda", compress = "bzip2")

complete.freqwords <- katherinemansfieldr::find_freq_char(mansfieldComplete)
punctList <-  c("\u2014", ",", ";", "!", "\\?", "\u201C", "...")
totalChapters <- katherinemansfieldr::find_chapters(mansfieldComplete)
completeAnalysisTable <- katherinemansfieldr::make_analysis_df(mansfieldComplete, totalChapters, complete.freqwords, punctList)

# find beginnings of stories
somethingChildish.years <- c(1908, 1913, 1913, 1913, 1914, 1915, 1915, 1917, 1917, 1917, 1917, 1910, 1917, 1917, 1917, 1919, 1921, 1921, 1910, 1910, 1910, 1911, 1912, 1912, 1913)
bliss.years <- c(1918, 1917, 1917, 1915, 1920, 1920, 1917, 1920, 1920, 1920, 1917, 1921, 1920, 1920)
gardenParty.years <- c(1922, 1921, 1920, 1921, 1920, 1921, 1920, 1922, 1921, 1921, 1920, 1921, 1921, 1921, 1920)

totalYears <- c(bliss.years, gardenParty.years, somethingChildish.years)
completeAnalysisTable <- mutate(completeAnalysisTable, years = totalYears, period = as.factor(ifelse(years < 1915, "early", ifelse(years < 1918, "middle", "late"))))

save(completeAnalysisTable, file = "completeAnalysisTable.rda", compress = "gzip")

