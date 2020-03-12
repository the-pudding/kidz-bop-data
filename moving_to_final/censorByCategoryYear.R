library(dplyr)
setwd("~/Desktop/kidz-bop-data/")


allC = read.csv("moving_to_final/data/allCensor.csv",stringsAsFactors = F)

## need category and year


badWords = read.csv("moving_to_final/data/badWord_categories_updated.csv", stringsAsFactors = F)
names(badWords)[3]="anchored"


tor=badWords %>% group_by(bad_word) %>% summarise(count = n()) %>% arrange(desc(count))

getrid=tor$bad_word[which(tor$count>=2)]

which(badWords$bad_word=="bang") ## 98
which(badWords$bad_word=="booty")  ## 115
which(badWords$bad_word=="bullet") ## 119
which(badWords$bad_word =="dope") ## 152
which(badWords$bad_word=="drink") ## 11
which(badWords$bad_word=="drug") ## 154
which(badWords$bad_word=="knife") ## 174
which(badWords$bad_word=="vodka") ## 230

badWords = badWords[-c(98, 115, 119, 152, 11, 154, 174, 230),]

allC=merge(allC, badWords[,1:2], by.x=c("badword"), by.y="bad_word", all.x=T)


