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
gardenParty <- scanMultiple(gardenParty.list)
somethingChildish <- scanMultiple(somethingChildish.list)
mansfieldComplete <- c(bliss, gardenParty, somethingChildish)

setwd("~/git/katherinemansfieldr/data")
save(bliss, file = "bliss.rda")
save(gardenParty, file = "gardenParty.rda")
save(somethingChildish, file = "somethingChildish.rda")
save(mansfieldComplete, file = "mansfieldComplete.rda")

complete.freqwords <- katherinemansfieldr::find_freq_char(mansfieldComplete)
punctList <-  c("—", ",", ";", "!", "\\?", "“", "...")
totalChapters <- katherinemansfieldr::find_chapters(mansfieldComplete)
totalChapters <- c(totalChapters, length(mansfieldComplete))
completeAnalysisTable <- katherinemansfieldr::make_analysis_df(mansfieldComplete, totalChapters, complete.freqwords, punctList)

# find beginnings of stories
somethingChildish.years <- c(1908, 1913, 1913, 1913, 1914, 1915, 1915, 1917, 1917, 1917, 1917, 1910, 1917, 1917, 1917, 1919, 1921, 1921, 1910, 1910, 1910, 1911, 1912, 1912, 1913)
bliss.years <- c(1918, 1917, 1917, 1915, 1920, 1920, 1917, 1920, 1920, 1920, 1917, 1921, 1920, 1920)
gardenParty.years <- c(1922, 1921, 1920, 1921, 1920, 1921, 1920, 1922, 1921, 1921, 1920, 1921, 1921, 1921, 1920)

totalYears <- c(bliss.years, gardenParty.years, somethingChildish.years)
completeAnalysisTable <- mutate(completeAnalysisTable, years = totalYears, period = as.factor(ifelse(years < 1915, "early", ifelse(years < 1918, "middle", "late"))))

save(completeAnalysisTable, file = "completeAnalysisTable.rda")
