library(geniusr)

genius_token() ## will prompt you for your API client, get one at https://genius.com/api-clients

library(dplyr)

kb <- search_artist("Kidz") ## helps us find the artist_id

## get all of their songs
kbSongs <- get_artist_songs(
  artist_id = 353676, include_features = FALSE,
  access_token = genius_token()
)

write.csv(kbSongs, "data/kbSongs.csv", row.names = F)

library(purrr)

safe_scrape <- safely(scrape_lyrics_id) ## make sure if we can't find a song's lyrics, it doesn't crash
kbLyrics <- map(kbSongs$song_id, ~ safe_scrape(.x)) ## patience, this takes awhile
save(kbLyrics, file = "data/kbLyrics.RData")




# not ready for the stuff below yet

## song id to feed into original lyrics
getOG <- lapply(kbSongs$song_name, function(x) {
  search_song(search_term = x) ## get it all, filter later
  # song_id used to get lyrics
  # artist_name
})
save(getOG, file = "data/OG_info_fromKBGenius.RData")

kbSongs$song_name

toCheck = cbind.data.frame(kb_song_name=kbSongs$song_name,do.call("rbind",lapply(getOG, function(x){x[1,]})),toCheck = ifelse(1:nrow(kbSongs) %in% geniusMatch$geniusIdx,1,0)) #%>% View() 

# only need to manually check if it is a match

names(toCheck)

forManualCheck = subset(toCheck[,c("kb_song_name", "song_name","artist_name","toCheck")], toCheck==1)

forManualCheck$isCorrect=rep("",nrow(forManualCheck))

write.csv(forManualCheck,"data/forManualChecking_getOriginalArtist.csv",row.names = F)
## if this isn't the match, I'll go back and find the backup
## once we get the correct song_id, get original lyrics

# ## get original lyrics
# ogLyrics <- lapply(unlist(getOG), function(x) {
#   scrape_lyrics_id(song_id = x)
# })
# save(ogLyrics, file = "ogLyrics.RData")
# 


## get original data for missing kidz bop songs just in case

extra <- read.csv("data/pulledSongs.csv",stringsAsFactors = F)

extra = subset(extra, included=="N")

extraNames = trimws(unlist(lapply(strsplit(extra$song_name, "\\("), function(x){x[1]})))

getOG <- lapply(extraNames, function(x) {
  search_song(search_term = x) ## get it all, filter later
  # song_id used to get lyrics
  # artist_name
})
save(getOG, file = "data/OG_info_Extra.RData")

kbSongs$song_name

toCheck = cbind.data.frame(kb_song_name=extraNames,do.call("rbind",lapply(getOG, function(x){x[1,]}))) #%>% View() 

# only need to manually check if it is a match



write.csv(toCheck,"data/forManualChecking_getOriginalArtistExtra.csv",row.names = F)
