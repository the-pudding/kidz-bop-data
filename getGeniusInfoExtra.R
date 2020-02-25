### get new genius
setwd("~/Desktop/kidz-bop-data")
extraKB = read.csv("data/missing/extraKB.csv",stringsAsFactors = F)
extraKB2 = read.csv("data/missing/extraKB2.csv",stringsAsFactors = F)

toGet = c(extraKB$song_id.x[1:118], extraKB2$song_id)
toGetS = c(extraKB$song_name.x[1:118], extraKB2$rack.name)

library(geniusr)

genius_token()
library(purrr)
safe_scrape <- safely(scrape_lyrics_id) ## make sure if we can't find a song's lyrics, it doesn't crash
kbLyricsExtra <- map(toGet, ~ safe_scrape(.x)) ## patience, this takes awhile
save(kbLyricsExtra, file = "data/missing/kbLyricsExtra.RData")

### get new original

getOGExtra <- lapply(toGetS, function(x) {
  search_song(search_term = x) ## get it all, filter later
  # song_id used to get lyrics
  # artist_name
})
save(getOGExtra, file = "data/missing/OG_info_extra.RData")
load("data/missing/OG_info_extra.RData")


cbind.data.frame(id = rep(1:length(getOGExtra),times = unlist(lapply(getOGExtra, nrow))), do.call("rbind", getOGExtra)) %>% View() 

rightOne = c(1, 11, 21, 33, 41,  51, 60, 71, 80, 87,
             97, 106, 117, 134, 135, 145, 157, 165,  179, 188,
             196, 205, 215, 224,  234, 244, 257, 264, 277, 284,
             294, 303, 313, 324, 333, 341, 351, 361, 371, 380, 
             393, 400, 406, 416, 426, 440, 446, 460, 465, 472, 
             482, 497, 500, 510, 520, 530, 540, 549, 559, 569,
             571, 581, 591, 607, 613, 621, 631, 639, 652, 659,
             669, 679, 689, 699, 709, 719, 729, 739, 750, 759,
             769, 779, 779, 801, 816,  820, 829, 839, 849, 859,
             870, 879, 889, 899, 909, 920, 935, 939, 949, 958,
             967, 977, 989, 997, 1006, 1017, 1026, 108, 109, 1056,
             1063, 1070, 1080, 1089, 1099, 1108, 1117, 1127
             )

toAdd = cbind.data.frame(id = rep(1:length(getOGExtra),times = unlist(lapply(getOGExtra, nrow))), do.call("rbind", getOGExtra))[rightOne,]

toAdd$rowid = 1:nrow(toAdd)
View(toAdd[,c(ncol(toAdd),1:3)])
#toAdd = toAdd[-82,] ## double nothing breaks like a heart

load( file = "data/missing/kbLyricsExtra.RData")
length(kbLyricsExtra)
kbLyricsExtra2 = kbLyricsExtra[-c(119:120)]


christmasEtc <- c(1, 4, 5, 16, 24, 29, 31, 33, 35, 43, 49, 52, 54, 63, 64, 68, 70, 71, 75, 80, 82, 87, 91, 92, 95, 98, 99, 102, 107, 108, 111, 116)

length(kbLyricsExtra2[-christmasEtc])

kbLyricsExtra = kbLyricsExtra2[-christmasEtc]
save(kbLyricsExtra, file="data/missing/kbLyricsExtraMatch.RData")

toAdd = toAdd[-christmasEtc,]

## update this, then grab og lyrics

## get extra stuff matched up 
## get year info

## can run differences when new words are ready

toAdd$song_id[grep("Happy Birthday",toAdd$song_name)] = 76302
toAdd$song_id[grep("Down",toAdd$song_name)] = 2433932
toAdd$song_id[grep("Cool",toAdd$song_name)] = 273786
toAdd$song_id[grep("Gotta Feeling",toAdd$song_name)] = 10692
toAdd$song_id[grep("MICKEY",toAdd$song_name)] = 846161
toAdd$song_id[grep("Over Your",toAdd$song_name)] = 392149
toAdd$song_id[grep("Paradise",toAdd$song_name)] = 3444501
toAdd$song_id[grep("Wild Ones",toAdd$song_name)] = 68266

safe_scrape <- safely(scrape_lyrics_id) ## make sure if we can't find a song's lyrics, it doesn't crash
ogLyricsExtra <- map(toAdd$song_id, ~ safe_scrape(.x)) ## patience, this takes awhile
save(ogLyricsExtra, file = "data/missing/ogLyricsExtra.RData")


# ditch first one anyway
# replace birthday with https://genius.com/The-beatles-birthday-lyrics 76302
# replace down with https://genius.com/Marian-hill-down-lyrics 2433932
# replace cool with https://genius.com/Gwen-stefani-cool-lyrics 273786
# fix i got a feeling https://genius.com/The-black-eyed-peas-i-gotta-feeling-lyrics 10692
# fix mickey https://genius.com/Toni-basil-mickey-12-version-lyrics  846161
# over you https://genius.com/Daughtry-over-you-lyrics 392149
# paradise https://genius.com/George-ezra-paradise-lyrics 3444501
# two nothing breaks like a heart?
# wild ones https://genius.com/Flo-rida-wild-ones-lyrics 68266
# it's your birthday, kidz bop shuffle, life of the party, sounds like a hit, is kb og
## ugh christmas, pb jelly time