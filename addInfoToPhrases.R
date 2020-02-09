setwd("~/Desktop/kidz-bop-data")
censoredPhrases = read.csv("data/censoring/setdiffPhrases.csv",stringsAsFactors = F)
library(geniusr)
genius_token() 
library(purrr)
library(dplyr)

#### og artist ####
load("data/2020-02-02/ogLyricsFullPlain.RData")
og_artist = unlist(lapply(ogLyricsFull, function(x){x$artist_name[1]}))

toM = cbind.data.frame(og_artist = og_artist, og_idx = 1:length(og_artist))
censoredPhrasesPlus = merge(censoredPhrases, toM, by.x="og_idx", by.y= "og_idx", all.x=T)

#### release date ####
load("data/2020-02-02/kbLyrics.RData")

test=get_song_meta(kbLyrics[[1]]$song_id[1])

kbSongID = unlist(lapply(kbLyrics, function(x){x$song_id[1]}))
ogSongID = unlist(lapply(ogLyricsFull, function(x){x$song_id[1]}))

safe_meta <- safely(get_song_meta) ## make sure if we can't find a song, it doesn't crash
ogDetails<- map(ogSongID, ~ safe_meta(.x))
kbDetails<- map(kbSongID, ~ safe_meta(.x))

og_release = unlist(lapply(ogDetails, function(x){x$result$release_date}))
kb_release = unlist(lapply(kbDetails, function(x){x$result$release_date})) ## some of these are missing

toM = cbind.data.frame(og_release = og_release, og_idx = 1:length(og_release))
toM2 = cbind.data.frame(kb_release = kb_release, kb_idx = 1:length(kb_release))

censoredPhrasesPlus2 = merge(censoredPhrasesPlus, toM, by.x="og_idx", by.y="og_idx",all.x=T)

censoredPhrasesPlus3 = merge(censoredPhrasesPlus2, toM2, by.x="kbID", by.y="kb_idx",all.x=T)

censoredPhrasesPlus3$og_artist = as.character(censoredPhrasesPlus3$og_artist)
write.csv(censoredPhrasesPlus3, "data/censoring/setdiffPhrasesPlus.csv",row.names=F)


censoredPhrasesPlus3$kb_song_name[which(is.na(censoredPhrasesPlus3$kb_release))] %>% unique() %>% length() ## 318, yikes

#### get og artist unique for manually entering gender ####

artists = censoredPhrasesPlus3 %>% group_by(og_artist) %>% summarise(count = n())


write.csv(artists[,1] , "data/2020-02-02/originalArtists.csv",row.names=F)
