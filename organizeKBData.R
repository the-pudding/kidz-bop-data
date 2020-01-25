# merge kidz bop lyrics to song info

load(file = "data/kbLyrics.RData")
load(file="data/spotifyData.RData") ## called all

kbSongs <- read.csv("data/kbSongs.csv",stringsAsFactors = F)

intersect(all$track.name,kbSongs$song_name) %>% length() ## 486 directly match genius

dim(all) ## out of total kidz bop songs 690

setdiff(all$track.name,kbSongs$song_name) %>% length() # 179 
## do we want to manually track these down?

setdiff(kbSongs$song_name, all$track.name) %>% length() # 235
## don't care about these

## what about fuzzy matching
helper <- function(pattern){
  agrep(pattern, kbSongs$song_name)[1]
}

tryThis = lapply(all$track.name,helper)

length(which(is.na(unlist(tryThis)))) ##  brings down to 84 non matches

cbind.data.frame(all$track.name,kbSongs$song_name[unlist(tryThis)]) %>% View()
## some obvious false hits, not sure if we want to go there

## direct match
helper <- function(pattern){
  grep(pattern, kbSongs$song_name)[1]
}

tryThis = lapply(all$track.name,helper)

#cbind.data.frame(all$track.name,kbSongs$song_name[unlist(tryThis)]) %>% View()

geniusMatch = cbind.data.frame(all,geniusSongName = kbSongs$song_name[unlist(tryThis)], geniusIdx = unlist(tryThis)) 

save(geniusMatch, file="data/kbMatch.RData")
