library(dplyr)
setwd("~/Desktop/kidz-bop-data/")

load(file="data/2020-02-02/ogLyricsFullPlain.RData") ## ogLyricsFull
load(file="data/2020-02-02/kbLyrics.RData") ## kbLyrics

censored = read.csv("data/censoring/fullDataSet.csv", stringsAsFactors = F)


pairs = censored %>% group_by(kbID, og_idx, kb_song_name, og_artist, og_release_year, kb_release_year) %>% summarise() 


checkLines = mapply( function(x,y){nrow(kbLyrics[[x]]) - nrow(ogLyricsFull[[y]])}, pairs$kbID, pairs$og_idx,SIMPLIFY = F) %>% unlist()

summary(checkLines)


pairs$difflines_KBOG = checkLines


extra = read.csv("data/missing/extraDates.csv", stringsAsFactors = F)

pairs2 = extra %>% group_by(storageId, song_name, ogRelease, kbRelease) %>% summarise() 

load(file = "data/missing/kbLyricsExtraMatch.RData")
load(file="data/missing/ogLyricsExtra.RData")

checkLines = lapply(pairs2$storageId, function(x){nrow(kbLyricsExtra[[x]]$result) - nrow(ogLyricsExtra[[x]]$result)}) 

checkLines = unlist(lapply(checkLines, function(x){ifelse(length(x)>0, x, NA)}))

pairs2$difflines_KBOG = checkLines
names(pairs)[3]="song_name"

all = rbind(pairs[,c("song_name","difflines_KBOG")], pairs2[,c("song_name","difflines_KBOG")])

summary(all$difflines_KBOG)

which(abs(all$difflines_KBOG)>20) %>% length()

write.csv(all, "data/censoring/lineDifferences.csv",row.names=F)
