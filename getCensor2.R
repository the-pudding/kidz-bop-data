library(dplyr)
setwd("~/Desktop/kidz-bop-data/")

badWords = read.csv("data/censoring/badWord_categories.csv", stringsAsFactors = F)

badWords = badWords[, c("bad_word", "category")] ## don't need preliminary counts
badWords = badWords[-1,] ## remove NA
badWords = badWords[-11,] ## remove double "drink"
badWords$anchored = paste0("^",badWords$bad_word, "$")

load(file="data/2020-02-02/ogLyricsFullPlain.RData") ## ogLyricsFull
load(file="data/2020-02-02/kbLyrics.RData") ## kbLyrics

censored = read.csv("data/censoring/fullDataSet.csv", stringsAsFactors = F)


pairs = censored %>% group_by(kbID, og_idx, kb_song_name, og_artist, og_release_year, kb_release_year) %>% summarise() 



helper_kb <- function(id, badword_vctr){
  tryThis = kbLyrics[[id]]$line
  tryThis = tolower(tryThis)
  
  tryThis = gsub("\\(", "", tryThis)
  tryThis = gsub("\\)", "", tryThis)
  ## take out punctuation
  tryThis = gsub("\\.", "", tryThis)
  tryThis = gsub("\\?", "", tryThis)
  tryThis = gsub("\\!", "", tryThis)
  tryThis = gsub(",", "", tryThis)
  tryThis = gsub("'", "", tryThis)
  tryThis = gsub("’", "", tryThis)
  # get rid of hyphens
  tryThis = gsub("-", "", tryThis)
  
  tryThis = unlist(strsplit(tryThis, " "))
  
  #browser()
  
  record = lapply(badword_vctr, function(x){length(grep(x,tryThis))})
  
  return(record)
}

helper_og <- function(id, badword_vctr){
  tryThis = ogLyricsFull[[id]]$line
  tryThis = tolower(tryThis)
  
  tryThis = gsub("\\(", "", tryThis)
  tryThis = gsub("\\)", "", tryThis)
  ## take out punctuation
  tryThis = gsub("\\.", "", tryThis)
  tryThis = gsub("\\?", "", tryThis)
  tryThis = gsub("\\!", "", tryThis)
  
  tryThis = gsub(",", "", tryThis)
  tryThis = gsub("'", "", tryThis)
  tryThis = gsub("’", "", tryThis)
  # get rid of hyphens
  tryThis = gsub("-", "", tryThis)
  
  tryThis = unlist(strsplit(tryThis, " "))
  
  
  record = lapply(badword_vctr, function(x){length(grep(x,tryThis))})
  
  return(record)
  
}

kbC = lapply(pairs$kbID,helper_kb, badWords$anchored)



ogC = lapply(pairs$og_idx,helper_og, badWords$anchored)


kbC2 = lapply(kbC, function(x){which(x!=0)})
ogC2 = lapply(ogC, function(x){which(x!=0)})

presentD = cbind.data.frame(og_idx = rep(pairs$og_idx,  lapply(ogC2, length)), bad_word_idx = unlist(ogC2), isPresent = T )#%>% length()

notcensoredD = cbind.data.frame(kbID = rep(pairs$kbID,  lapply(kbC2, length)), bad_word_idx = unlist(kbC2), isStillPresent = T )


#lapply(kbC2, length) %>% unlist %>% sum()

censoredW = mapply(function(x,y){setdiff(y,x)}, kbC2, ogC2, SIMPLIFY = F)
notCensoredW = mapply(function(x,y){intersect(y,x)}, kbC2, ogC2, SIMPLIFY = F)


#rep(badWords$bad_word, nrow(pairs)) %>% length()

test = do.call("rbind", replicate(nrow(badWords),pairs, simplify = FALSE))

full = test
full$bad_word = rep(badWords$bad_word, nrow(pairs))
full$bad_word_idx = rep(1:nrow(badWords),nrow(pairs))

tryThis = merge(presentD, full, by.x= c("og_idx", "bad_word_idx"), by.y = c("og_idx", "bad_word_idx"),all.x = T, all.y=T)

tryThis$isPresent[is.na(tryThis$isPresent)]= F

tryThis2 = merge(notcensoredD, tryThis,by.x= c("kbID", "bad_word_idx"), by.y = c("kbID", "bad_word_idx"),all.x = T, all.y=T )

tryThis2$isStillPresent[is.na(tryThis2$isStillPresent)]= F

tryThis2$isCensored = tryThis2$isPresent & !tryThis2$isStillPresent

tryThis2= merge(tryThis2,badWords[,1:2],by.x="bad_word", by.y="bad_word" )


write.csv(tryThis2, file="data/censoring/fullDataSet.csv",row.names=F)

firstPresent = tryThis2 %>% filter(isPresent) %>% group_by(bad_word) %>% summarise(firstAppear = min(kb_release_year), numPresent = n()) 

firstCensored = tryThis2 %>% filter(isCensored) %>% group_by(bad_word) %>% summarise(firstCensored = min(kb_release_year), numCensored = n()) #%>% dim()

firstPresent$firstCensored=firstCensored$firstCensored
firstPresent$numCensored=firstCensored$numCensored

head(firstPresent)

firstPresent$bad_word[which(firstPresent$firstCensored> firstPresent$firstAppear)]

firstPresent = merge(firstPresent,badWords[,1:2],by.x="bad_word", by.y="bad_word" )

write.csv(firstPresent[,c("bad_word","category","firstAppear","firstCensored","numPresent","numCensored")], file="data/censoring/censoringTimes.csv",row.names=F)
