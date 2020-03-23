library(dplyr)
setwd("~/Desktop/kidz-bop-data/")

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

load(file = "data/2020-02-02/kbLyrics.RData")
load(file = "data/2020-02-02/ogLyricsFullPlain.RData")
load(file="data/missing/ogLyricsExtra.RData")
load(file="data/missing/kbLyricsExtraMatch.RData")
crosswalk = read.csv("data/2020-02-02/crosswalk.csv", stringsAsFactors = F)

helper_kb <- function(id, badword){
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
  
  record = length(grep(badword,tryThis))
  
  return(record)
}

kbC = mapply(helper_kb, rep(crosswalk$kb_idx,nrow(badWords)),rep(badWords$anchored, each = nrow(crosswalk)))

length(unlist(kbC)) == length(rep(crosswalk$kb_idx,nrow(badWords)))

kbCensored = cbind.data.frame(kb_idx = rep(crosswalk$kb_idx,nrow(badWords)), badword = rep(badWords$bad_word, each = nrow(crosswalk)), numOccurKB = unlist(kbC) )
kbCensored$id = rep(1:nrow(crosswalk), nrow(badWords))

helper_og <- function(id, badword){
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
  
  
  record = length(grep(badword,tryThis))
  
  return(record)
  
}

ogC = mapply(helper_og, rep(crosswalk$og_idx,nrow(badWords)),rep(badWords$anchored, each = nrow(crosswalk)))

length(unlist(ogC)) == length(rep(crosswalk$og_idx,nrow(badWords)))

ogCensored = cbind.data.frame(og_idx = rep(crosswalk$og_idx,nrow(badWords)), badword = rep(badWords$bad_word, each = nrow(crosswalk)), numOccurOG = unlist(ogC))
ogCensored$id =  rep(1:nrow(crosswalk), nrow(badWords))

## need song title and artist

song_name = lapply(rep(crosswalk$kb_idx,nrow(badWords)), function(x){kbLyrics[[x]]$song_name[1]})
song_name2 = lapply(rep(crosswalk$og_idx,nrow(badWords)), function(x){ogLyricsFull[[x]]$song_name[1]})

og_artist = lapply(rep(crosswalk$og_idx,nrow(badWords)), function(x){ogLyricsFull[[x]]$artist_name[1]})

length(unlist(song_name)) == length(rep(crosswalk$kb_idx,nrow(badWords)))
length(unlist(og_artist)) == length(rep(crosswalk$og_idx,nrow(badWords)))

length(unlist(lapply(song_name, function(x){ifelse(is.null(x),NA, x)})))== length(rep(crosswalk$kb_idx,nrow(badWords)))

length(unlist(lapply(og_artist, function(x){ifelse(is.null(x),NA, x)})))== length(rep(crosswalk$kb_idx,nrow(badWords)))

song_name = unlist(lapply(song_name, function(x){ifelse(is.null(x),NA, x)}))
song_name2 = unlist(lapply(song_name2, function(x){ifelse(is.null(x),NA, x)}))

og_artist = unlist(lapply(og_artist, function(x){ifelse(is.null(x),NA, x)}))

helperSN = function(x,y){
  ifelse(is.na(x),y,x)
}
song_nameF = mapply(helperSN, song_name, song_name2)

kbCensored$song_name=song_nameF
kbCensored$og_artist = og_artist

all = merge(kbCensored, ogCensored, by.x=c("id", "badword"), by.y = c("id", "badword"))


#### extra ####
load(file="data/missing/ogLyricsExtra.RData")
load(file="data/missing/kbLyricsExtraMatch.RData")

helper_kb <- function(id, badword){
  tryThis = kbLyricsExtra[[id]]$result$line
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
  
  record = length(grep(badword,tryThis))
  
  return(record)
}
helper_og <- function(id, badword){
  tryThis = ogLyricsExtra[[id]]$result$line
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
  
  
  record = length(grep(badword,tryThis))
  
  return(record)
  
}

kbC2 = mapply(helper_kb, rep(1:length(kbLyricsExtra),nrow(badWords)),rep(badWords$anchored, each = length(kbLyricsExtra)))

length(unlist(kbC2)) == length(rep(1:length(kbLyricsExtra),nrow(badWords)))

kbCensored2 = cbind.data.frame(kb_idx = rep(1:length(kbLyricsExtra),nrow(badWords)), badword = rep(badWords$bad_word, each = length(kbLyricsExtra)), numOccurKB = unlist(kbC2) )
kbCensored2$id = rep(1:length(kbLyricsExtra), nrow(badWords))

ogC2 = mapply(helper_og, rep(1:length(ogLyricsExtra),nrow(badWords)),rep(badWords$anchored, each = length(ogLyricsExtra)))

length(unlist(ogC2)) == length(rep(1:length(ogLyricsExtra),nrow(badWords)))

ogCensored2 = cbind.data.frame(og_idx = rep(1:length(ogLyricsExtra),nrow(badWords)), badword = rep(badWords$bad_word, each = length(ogLyricsExtra)), numOccurOG = unlist(ogC2))
ogCensored2$id =  rep(1:length(ogLyricsExtra), nrow(badWords))

## need song title and artist

song_name = lapply(rep(1:length(kbLyricsExtra),nrow(badWords)), function(x){kbLyricsExtra[[x]]$result$song_name[1]})
song_name2 = lapply(rep(1:length(kbLyricsExtra),nrow(badWords)), function(x){ogLyricsExtra[[x]]$result$song_name[1]})
og_artist = lapply(rep(1:length(ogLyricsExtra),nrow(badWords)), function(x){ogLyricsExtra[[x]]$result$artist_name[1]})

#length(unlist(song_name)) == length(rep(1:length(kbLyricsExtra),nrow(badWords)))
#length(unlist(og_artist)) == length(rep(crosswalk$og_idx,nrow(badWords)))

length(unlist(lapply(song_name, function(x){ifelse(is.null(x),NA, x)})))== length(rep(1:length(kbLyricsExtra),nrow(badWords)))

length(unlist(lapply(og_artist, function(x){ifelse(is.null(x),NA, x)})))== length(rep(1:length(ogLyricsExtra),nrow(badWords)))

song_name = unlist(lapply(song_name, function(x){ifelse(is.null(x),NA, x)}))
song_name2 = unlist(lapply(song_name2, function(x){ifelse(is.null(x),NA, x)}))

og_artist = unlist(lapply(og_artist, function(x){ifelse(is.null(x),NA, x)}))

helperSN = function(x,y){
  ifelse(is.na(x),y,x)
}
song_nameF = mapply(helperSN, song_name, song_name2)


kbCensored2$song_name=song_name
kbCensored2$og_artist = og_artist

all2 = merge(kbCensored2, ogCensored2, by.x=c("id", "badword"), by.y = c("id", "badword"))

all2 %>% group_by(id) %>% summarise(count=n())

all$data = rep("first",nrow(all))
all2$data = rep("second",nrow(all2))

allF = rbind.data.frame(all, all2)

write.csv(allF[,-1],"moving_to_final/data/allCensor2.csv",row.names=F)

