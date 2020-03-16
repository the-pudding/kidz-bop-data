library(dplyr)
setwd("~/Desktop/kidz-bop-data/")


#allC = read.csv("moving_to_final/data/allCensor.csv",stringsAsFactors = F)
allC = read.csv("moving_to_final/data/allCensorF.csv",stringsAsFactors =F)

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

eachSong = allC %>% group_by(song_name, og_artist) %>% summarise(year = year[1])

eachSong[which(is.na(eachSong$year)),]

#write.csv(eachSong[which(is.na(eachSong$year)),],"moving_to_final/missingYearsF.csv", row.names=F)

#my = read.csv("moving_to_final/missingYears.csv", stringsAsFactors = F)
my = read.csv("moving_to_final/missingYearsF.csv", stringsAsFactors = F)

test = eachSong[which(is.na(eachSong$year)),]
test$year = my$year

allC2 = merge(allC, test, by.x=c("song_name","og_artist"), by.y=c("song_name","og_artist"),all.x=T)

#View(allC2)

allC2$year = ifelse(is.na(allC2$year.x), allC2$year.y, allC2$year.x)

idx = grep("/",allC2$year) 
allC2$year[idx]=paste0("20",unlist(lapply(strsplit(grep("/",allC2$year, value = T), "/" ), function(x){x[3]}))) #%>% unlist() %>% length()

write.csv(allC2, "moving_to_final/data/proportions-kb-prep.csv", row.names=F)


###

all = read.csv("moving_to_final/data/proportions-kb-prep.csv", stringsAsFactors = F)


all$isCensored = ifelse(all$numOccurKB < all$numOccurOG, 1, 0)
all$isPresent = ifelse(all$numOccurOG>0, 1, 0)

require(ggplot2)
all %>% group_by(year) %>% summarise(prop=sum(isCensored)/n()) %>% ggplot(., aes(year, prop))+geom_point()

all$category = ifelse(all$category %in% c("profanity","slur"), "profanity", all$category)
all$category = ifelse(all$category %in% c("gender & sexuality"), "identity", all$category)
all$category = ifelse(all$category %in% c("gender & sexuality"), "identity", all$category)
all$category = ifelse(all$category %in% c("religious","mental health","other"), "other", all$category)


byYearGroup= all %>% group_by(year,category) %>% summarise(totalCensoredG= sum(isCensored), totalExistG = sum(isPresent)) 

propCensoredByYear = all %>% group_by(year) %>% summarise(propCensored = sum(isCensored)/sum(isPresent), totalCensored = sum(isCensored))

test = merge(byYearGroup, propCensoredByYear, by.x="year", by.y="year")
test$tryThis = test$totalCensoredG/test$totalCensored * test$propCensored

test2=subset(test, year ==2001)

sum(test2$tryThis)


require(tidyr)

#spread(test[,c("year","category","tryThis")], category, tryThis) %>% View()
apply(spread(test[,c("year","category","tryThis")], category, tryThis)[,-1] , 1, sum)

propCensoredByYear$propCensored

tidyV = spread(test[,c("year","category","tryThis")], category, tryThis)
tidyV = tidyV[,c("year","alcohol & drugs","sexual","profanity","violence","identity","other")]
names(tidyV)[2]="alcohol"

tidyV$alcohol = tidyV$alcohol *100
tidyV$sexual = tidyV$sexual *100
tidyV$profanity = tidyV$profanity *100
tidyV$identity = tidyV$identity *100
tidyV$other = tidyV$other *100

write.csv(tidyV, "moving_to_final/data/proportions-kb.csv", row.names=F)


ggplot(test, aes(year, total))+geom_point()
