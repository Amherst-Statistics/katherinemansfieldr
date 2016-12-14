scanMultiple <- function(list){
  output <- c()
  for(i in 1:length(list)){
    story <- scan(list[i], what = "character", sep = "\n")
    output <- c(output, story)
  }
  return(output)
}
setwd("~/katherinemansfieldr/data-raw")
bliss.list <- list.files(pattern = "bliss\\..+\\.txt")
gardenParty.list <- list.files(pattern = "gardenParty\\..+\\.txt")
somethingChildish.list <- list.files(pattern = "somethingChildish\\..+\\.txt")

# create the collection texts
bliss <- scanMultiple(bliss.list)
gardenParty <- scanMultiple(gardenParty.list)
somethingChildish <- scanMultiple(somethingChildish.list)
mansfieldComplete <- c(bliss, gardenParty, somethingChildish)

setwd("~/katherinemansfieldr/data")
save(bliss, file = "bliss.rda")
save(gardenParty, file = "gardenParty.rda")
save(somethingChildish, file = "somethingChildish.rda")
save(mansfieldComplete, file = "mansfieldComplete.rda")
