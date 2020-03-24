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

load(file = "moving_to_final/data/kbReplacementLyrics.RData")
newOG = kbLyrics
load(file = "data/2020-02-02/kbLyrics.RData")
#load(file = "data/2020-02-02/ogLyricsFullPlain.RData")
#load(file="data/missing/ogLyricsExtra.RData") ## all first

toReplace = read.csv("moving_to_final/data/toReplace.csv", stringsAsFactors = F)


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

kbC = mapply(helper_kb, rep(toReplace$kb_idx,nrow(badWords)),rep(badWords$anchored, each = nrow(toReplace)))

length(unlist(kbC)) == length(rep(toReplace$kb_idx,nrow(badWords)))

kbCensored = cbind.data.frame(kb_idx = rep(toReplace$kb_idx,nrow(badWords)), badword = rep(badWords$bad_word, each = nrow(toReplace)), numOccurKB = unlist(kbC) )
kbCensored$id = rep(1:nrow(toReplace), nrow(badWords))

helper_og <- function(id, badword){
  tryThis = newOG[[id]]$result$line
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

ogC = mapply(helper_og, rep(1:nrow(toReplace),nrow(badWords)),rep(badWords$anchored, each = nrow(toReplace)))

length(unlist(ogC)) == length(rep(toReplace$og_idx,nrow(badWords)))

ogCensored = cbind.data.frame(og_idx = rep(1:nrow(toReplace),nrow(badWords)), badword = rep(badWords$bad_word, each = nrow(toReplace)), numOccurOG = unlist(ogC))
ogCensored$id =  rep(1:nrow(toReplace), nrow(badWords))

## need song title and artist

song_name = lapply(rep(toReplace$kb_idx,nrow(badWords)), function(x){kbLyrics[[x]]$song_name[1]})
song_name2 = lapply(rep(1:nrow(toReplace),nrow(badWords)), function(x){newOG[[x]]$result$song_name[1]})

og_artist = lapply(rep(1:nrow(toReplace),nrow(badWords)), function(x){newOG[[x]]$result$artist_name[1]})



length(unlist(lapply(song_name, function(x){ifelse(is.null(x),NA, x)})))== length(rep(toReplace$kb_idx,nrow(badWords)))

length(unlist(lapply(og_artist, function(x){ifelse(is.null(x),NA, x)})))== length(rep(toReplace$kb_idx,nrow(badWords)))

song_name = unlist(lapply(song_name, function(x){ifelse(is.null(x),NA, x)}))
song_name2 = unlist(lapply(song_name2, function(x){ifelse(is.null(x),NA, x)}))

og_artist = unlist(lapply(og_artist, function(x){ifelse(is.null(x),NA, x)}))

helperSN = function(x,y){
  ifelse(is.na(x),y,x)
}
song_nameF = mapply(helperSN, song_name, song_name2)

kbCensored$song_name=song_nameF
kbCensored$og_artist = og_artist

allC = merge(kbCensored, ogCensored, by.x=c("id", "badword"), by.y = c("id", "badword"))

####

library(dplyr)
setwd("~/Desktop/kidz-bop-data/")


#allC = read.csv("moving_to_final/data/allCensor.csv",stringsAsFactors = F)
#allC = read.csv("moving_to_final/data/allCensorF2.csv",stringsAsFactors =F)

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

allC=merge(allC, badWords[,1:3], by.x=c("badword"), by.y="bad_word", all.x=T)
allC$song_name = tolower(allC$song_name)

datesI=read.csv("data/censoring/fullDataSet.csv",stringsAsFactors = F)

setdiff(unique(allC$song_name), datesI$kb_song_name)

datesM = read.csv("data/missing/extraDates.csv", stringsAsFactors = F)
datesM$song_name= tolower(datesM$song_name)
#testM = merge(allC, datesI, by.x="song_name", by.y="kb_song_name",all.x=T)

setdiff(unique(allC$song_name), c(datesI$kb_song_name, tolower(datesM$song_name)))

helper <- function(x){
  try1=which(datesI$kb_song_name == x)
  try2=which(datesM$song_name == x)
  #print(x)
  if(length(try1)==0){
    if(length(try2)==0){
      return(NA)
    }else{
      return(datesM[try2[1],"kb_release_date"])
    }
  }else{
    return(datesI[try1[1], "kb_release_year"])
  }
  
}

tryThis  = lapply(allC$song_name, helper)

allC$year = unlist(tryThis)

#eachSong = allC %>% group_by(song_name, og_artist) %>% summarise(year = year[1])

#eachSong[which(is.na(eachSong$year)),]

#write.csv(eachSong[which(is.na(eachSong$year)),],"moving_to_final/missingYearsF2.csv", row.names=F)

#my = read.csv("moving_to_final/missingYears.csv", stringsAsFactors = F)
#my = read.csv("moving_to_final/missingYearsF2.csv", stringsAsFactors = F)

#test = eachSong[which(is.na(eachSong$year)),]
#test$year = my$year

#allC2 = merge(allC, test, by.x=c("song_name","og_artist"), by.y=c("song_name","og_artist"),all.x=T)

#View(allC2)

#allC2$year = ifelse(is.na(allC2$year.x), allC2$year.y, allC2$year.x)

#idx = grep("/",allC2$year) 
#allC2$year[idx]=paste0("20",unlist(lapply(strsplit(grep("/",allC2$year, value = T), "/" ), function(x){x[3]}))) #%>% unlist() %>% length()

write.csv(allC, "moving_to_final/data/proportions-kb-prepAddOn.csv", row.names=F)
