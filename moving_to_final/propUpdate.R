setwd("~/Desktop/kidz-bop-data")
library(dplyr)
all = read.csv("moving_to_final/data/proportions-kb-prepF.csv", stringsAsFactors = F)
## add in prepAddOn



addon = read.csv("moving_to_final/data/proportions-kb-prepAddOn.csv", stringsAsFactors = F)
addon = addon[,-2] ## minus id

all2 = rbind.data.frame(all[,names(addon)] , addon)

fix = all2 %>% group_by(song_name, og_artist) %>% summarise(count=n()) %>% arrange(desc(count)) %>% as.data.frame() %>% head(21)

## gotta kill duplicates



ch1=c()
ch2=c()
for(i in 1:nrow(fix)){
  test=subset(all2, song_name==fix$song_name[i] & og_artist == fix$og_artist[i]) %>% group_by(badword) %>% summarise(check1=numOccurKB[1]==numOccurKB[2], check2=numOccurOG[1] == numOccurOG[2])
  ch1=c(ch1,sum(test$check1))
  ch2=c(ch2,sum(test$check2))
}

all2=all2[-which(all2$og_idx==566),]
## can just delete one of each

all3=all2 %>% group_by(song_name, og_artist, badword, category, anchored, year) %>% summarise(kb_idx=kb_idx[1], og_idx = og_idx[1], numOccurKB=numOccurKB[1], numOccurOG=numOccurOG[1])
#all3 %>% group_by(song_name, og_artist) %>% summarise(count=n()) %>% arrange(desc(count))

all = all3

all$isCensored = ifelse(all$numOccurKB < all$numOccurOG, 1, 0)
all$isPresent = ifelse(all$numOccurOG>0, 1, 0)

toRemove = c(which(all$og_artist=="Blue Swede"),
which(all$og_artist=="Daddy Yankee"),
which(all$og_artist=="Kerstin Ott & Helene Fischer"),
which(all$og_artist=="Kesha" & all$song_name=="tik tok"),
which(all$og_artist=="Poison"),
which(all$og_artist=="Miley Cyrus" & is.na(all$song_name)),
which(all$og_artist=="New Edition"),
which(all$og_artist=="The Beatles"),
which(all$og_artist=="The Rembrandts"),
which(all$og_artist=="Toni Basil"),
which(all$og_artist=="LunchMoney Lewis"))

all[which(all$og_artist=="Flo Rida" & all$song_name=="wild ones" & all$badword=="high"),] ## censored once

all[which(all$og_artist=="Jason Derulo" & all$song_name=="it girl" & all$badword=="crazy"),"isCensored"]=0 

all[which(all$og_artist=="Jessie J" & all$song_name=="price tag" & all$badword=="man"),"isCensored"]=0

all[which(all$og_artist=="Kelly Clarkson" & all$song_name=="miss independent" & all$badword=="man"),"isCensored"]=0

all[which(all$og_artist=="Meghan Trainor" & all$song_name=="me too" & all$badword=="god"),"isCensored"]=0


all[which(all$og_artist=="One Direction" & all$song_name=="drag me down" & all$badword=="man"),"isCensored"]=0

all[which(all$og_artist=="Taylor Swift" & all$song_name=="bad blood" & all$badword=="blood"),"isCensored"]=0

all[which(all$og_artist=="Whitney Houston" & all$song_name=="my love is your love" & all$badword=="lord"),"isCensored"]=0

all[which(all$og_artist=="Whitney Houston" & all$song_name=="my love is your love" & all$badword=="war"),"isCensored"]=0

all = all[-toRemove,]

require(ggplot2)
all %>% group_by(year) %>% summarise(prop=sum(isCensored)/n()) %>% ggplot(., aes(year, prop))+geom_point()

all$category = ifelse(all$category %in% c("profanity","slur"), "profanity", all$category)
all$category = ifelse(all$category %in% c("gender & sexuality"), "identity", all$category)
all$category = ifelse(all$category %in% c("gender & sexuality"), "identity", all$category)
all$category = ifelse(all$category %in% c("religious","mental health","other"), "other", all$category)

notC = read.csv("moving_to_final/data/notC.csv", stringsAsFactors = F)

helper <- function(idx){
  which(all$kb_idx == notC$kb_idx[idx] & all$og_idx==notC$og_idx[idx] & all$badword==notC$badword[idx])#[1]
}
toRemove = lapply(1:nrow(notC), helper)
lapply(toRemove, length) %>% unlist() %>% summary()

toRemove = unlist(toRemove)[which(!is.na(unlist(toRemove)))]

all2 = all[-toRemove,]

#all3 = all2[-which(is.na(all2$year)),] 

all = all2

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

write.csv(tidyV, "moving_to_final/data/proportions-kbF.csv", row.names=F)


ggplot(propCensoredByYear, aes(year, propCensored))+geom_point()
