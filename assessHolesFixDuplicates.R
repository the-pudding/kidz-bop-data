setwd("/Users/Sara/Desktop/kidz-bop-data")
library(dplyr)

load(file="data/2020-02-02/kbLyrics.RData")
data = read.csv("data/censoring/fullDataSet.csv",stringsAsFactors = F)
View(data)

data2 = data[-which(data$kbID==308 & data$og_idx == 295),]



## rob thomas has two og IDs

## only 76 possible

subset(data2, kbID==215) %>% View()

kbLyrics[[215]] ## enrique

data3 = data2[-which(data2$kbID==215 & data2$og_artist=="Chad Kroeger" ),]

subset(data3, kbID==215) %>% group_by(bad_word_idx) %>% summarise(count=n(), bad_word) %>% arrange(desc(count))

data4=data3[!duplicated(data3),]

kbSongs = data4 %>% group_by(kbID, kb_song_name, og_artist) %>% summarise(count = n()) %>% arrange(desc(count))



kbLyrics[[14]] ## POD

data5 = data4[-which(data4$kbID==14 & data4$og_artist=="Krewella"),]



kbLyrics[[172]] ## Sugar Ray

data6 = data5[-which(data5$kbID==172& data5$og_artist=="Nicki Minaj"),]


kbSongs = data6 %>% group_by(kbID, kb_song_name) %>% summarise(count = n()) %>% arrange(desc(count))

kbLyrics[[184]] ## avril

data7 = data6[-which(data6$kbID==184 & data6$og_artist == "*NSYNC"),]


kbLyrics[[294]] ## hailee

data8 = data7[-which(data7$kbID==294 & data7$og_artist == "3 Doors Down"),]


kbLyrics[[452]] ## biebs

data9 = data8[-which(data8$kbID==452 & data8$og_artist == "Buckcherry"),]



kbLyrics[[503]] ## maren

data10 = data9[-which(data9$kbID==503 & data9$og_artist == "Jimmy Eat World"),]

kbSongs = data10 %>% group_by(kbID, kb_song_name) %>% summarise(count = n()) %>% arrange(desc(count))

kbSongs = data10 %>% group_by(og_idx, kb_song_name) %>% summarise(count = n()) %>% arrange(desc(count))


kbAll = read.csv("data/2020-02-02/kbSongNames.csv", stringsAsFactors = F)
kbAll$kb_song_name = tolower(kbAll$kb_song_name)
setdiff(kbAll$kb_song_name, kbSongs$kb_song_name) ## missing 18 songs from kbSongNames, now what is missing before that stage

#setdiff( kbSongs$kb_song_name, kbAll$kb_song_name)

write.csv(data10,"data/censoring/fullDataSet.csv", row.names=F )
