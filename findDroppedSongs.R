setwd("/Users/Sara/Desktop/kidz-bop-data")
library(dplyr)

load(file="data/2020-02-02/kbLyrics.RData")
load(file="data/2020-02-02/ogLyricsFullPlain.RData")
data = read.csv("data/censoring/fullDataSet.csv",stringsAsFactors = F)


pulled <- read.csv("data/pulledSongs.csv",stringsAsFactors = F)


kbAll = read.csv("data/2020-02-02/kbSongNames.csv", stringsAsFactors = F)
kbAll$kb_song_name = tolower(kbAll$kb_song_name)
setdiff(kbAll$kb_song_name, kbSongs$kb_song_name) ## missing 18 songs from kbSongNames, now what is missing before that stage

pulled$song_name = tolower(pulled$song_name)


intersect(pulled$song_name,data$kb_song_name) %>% length()
 
unique(data$og_idx) %>% length()


setdiff(pulled$song_name, data$kb_song_name) %>% length() ## 223


lapply(setdiff(1:length(kbLyrics),unique(data$kbID)), function(x){kbLyrics[[x]]$song_name[1]}) %>% unlist()

setdiff(kbAll$kb_song_name, kbSongs$kb_song_name)  ## so we have kidz bop lyrics for these

intersect(lapply(setdiff(1:length(ogLyricsFull),data$og_idx), function(x){ogLyricsFull[[x]]$song_name[1]}) %>% unlist() %>% tolower(),setdiff(pulled$song_name, data$kb_song_name) ) %>% length() ## 74 have og lyrics for

#### available from kidz bop v. pulled data ####
kbGeniusAvailable = read.csv("data/kbSongs.csv", stringsAsFactors = F)
kbGeniusAvailable$song_name = tolower(kbGeniusAvailable$song_name)

songName_need = setdiff(pulled$song_name, data$kb_song_name) ## ones we don't have yet
pulled$id = 1:nrow(pulled)
pulledID_need = which(pulled$song_name %in% songName_need)


matches = lapply(songName_need, function(x){grep(x, kbGeniusAvailable$song_name)})

which(unlist(lapply(matches, length) )>0) %>% length() ## 123 have matches

id = rep(which(unlist(lapply(matches, length) )>0), times = unlist(lapply(matches, length))[which(unlist(lapply(matches, length) )>0)]) ## ids for songName_need

pulledID_need[id] 

kbGeniusAvailable$id = 1:nrow(kbGeniusAvailable)

songName = unlist(lapply(matches, function(x){kbGeniusAvailable[x,"song_name"]}))
songID = unlist(lapply(matches, function(x){kbGeniusAvailable[x,"id"]}))

kbGeniusAvailable[songID,] %>% dim()

matchInfo = cbind.data.frame(id = id, kbGeniusAvailable[songID,], pulledID = pulledID_need[id] )

tryThis = merge(pulled,matchInfo,by.x="numId", by.y="pulledID",all.x=F, all.y=T)

tryThis[,c("numId","song_name.y")] %>% View()

toRemove = c(3, 7, 17, 23, 25, 33:36, 43, 45, 46:49, 98, 109, 110, 115, 117, 133, 134, 138, 139 )

tryThis2 = tryThis[-toRemove,]

tryThis2[,c("numId", "song_name.y")] %>% group_by(numId) %>% summarise(count=n()) %>% arrange(desc(count))

tryThis2[,c("numId","song_name.y")] %>% View()

toRemove2 = c(99, 144, 145)

tryThis3= tryThis2[-toRemove2,]

tryThis4 = tryThis3[-c(144:146),]

tryThis4$song_id.y ## kidz bop ids

write.csv(tryThis4,"data/missing/extraKB.csv",row.names=F)


needed = setdiff(pulled$song_name, c(data$kb_song_name, tryThis4$song_name.x))

extra = read.csv("data/2020-02-02/DiscographyManualCheck-missingLyrics.csv", stringsAsFactors = F)

extra$track.name = tolower(extra$track.name)

intersect(needed,extra$track.name) %>% length()

extraId = c()
for(i in 1:length(needed)){
 extraId = c(extraId, which(extra$track.name==needed[i]))
}

extraInfo = extra[extraId,]

write.csv(subset(extraInfo,!is.na(extraInfo$song_id)), "data/missing/extraKB2.csv",row.names=F)

#### just need to match these up ####
intersect(lapply(setdiff(1:length(ogLyricsFull),data$og_idx), function(x){ogLyricsFull[[x]]$song_name[1]}) %>% unlist() %>% tolower(), setdiff(kbAll$kb_song_name, kbSongs$kb_song_name) ) %>% length() ## kidz bop + og for 9 extra

toGet = intersect(lapply(setdiff(1:length(ogLyricsFull),data$og_idx), function(x){ogLyricsFull[[x]]$song_name[1]}) %>% unlist() %>% tolower(), setdiff(kbAll$kb_song_name, kbSongs$kb_song_name) ) 


ogLyricID = setdiff(1:length(ogLyricsFull),data10$og_idx)
ogSongName = lapply(setdiff(1:length(ogLyricsFull),data$og_idx), function(x){ogLyricsFull[[x]]$song_name[1]})

ogLyricID_idx = which(unlist(lapply(ogSongName, length))>0)

ogLyricID_name = unlist(ogSongName)

ogIds = c()
kbAllIds = c()
for(i in 1:length(toGet)){
  ogIds = c(ogIds,which( tolower(unlist(lapply(ogLyricsFull, function(x){x$song_name[1]}))) ==toGet[i]))
  kbAllIds = c(kbAllIds,which(kbAll$kb_song_name ==toGet[i]))
}



kbAll[kbAllIds,]
kbLyrics[[20]]
ogLyricsFull[[22]]

kb_id = kbAllIds
og_id = ogIds
song_name = lapply(og_id, function(x){ogLyricsFull[[x]]$song_name[1]}) %>% unlist()
og_artist = lapply(og_id, function(x){ogLyricsFull[[x]]$artist_name[1]}) %>% unlist()

extra = cbind.data.frame(kb_id, og_id, song_name, og_artist)
write.csv(extra,"data/missing/newHaveLyricsAlready.csv",row.names=F)
