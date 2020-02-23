library(dplyr)

setwd("~/Desktop/kidz-bop-data")
phrases = read.csv("data/censoring/setdiffPhrases.csv",stringsAsFactors = F)

View(phrases)


test = lapply(phrases$song, function(x){strsplit(x," ")})
test2 = unlist(test)     
test3 = tolower(test2)

test = gsub("\\(", "", test3)
test = gsub("\\)", "", test)
## take out punctuation
test = gsub("\\.", "", test)
test = gsub("\\?", "", test)
test = gsub("\\!", "", test)
test = gsub(",", "", test)
test= gsub("'", "", test)
test = gsub("’", "",test)
# get rid of hyphens
test = gsub("-", "", test)
test = gsub("\n", " ", test)
test = gsub("\\+", " ", test)

test2 = unlist(strsplit(test, " "))

test3 = data.frame(word = test2)

test4 = test3 %>% group_by(word) %>% summarise(count = n()) %>% arrange(desc(count))
test5 = test3 %>% group_by(word) %>% summarise(count = n()) %>% arrange(count)


write.csv(test4, "data/censoring/extraCensor.csv",row.names=F)

extraCensor = read.csv("data/censoring/extraCensor.csv",stringsAsFactors = F) ## manual tagging


check = subset(extraCensor, doesItMatter==1)

