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

# ## song id to feed into lyrics
# getOG <- lapply(kbSongs$song_name, function(x) {
#   search_song(search_term = x)$song_id[1]
# })
# save(getOG, file = "OG_artist_KB.RData")
# 
# ## get original lyrics
# ogLyrics <- lapply(unlist(getOG), function(x) {
#   scrape_lyrics_id(song_id = x)
# })
# save(ogLyrics, file = "ogLyrics.RData")
# 
# ## get original artist for reference
# ## note we could be getting the wrong one, but we are hoping the first one is the right one
# getOGartist <- lapply(kbSongs$song_name, function(x) {
#   search_song(search_term = x)$artist_name[1]
# })
# save(getOGartist, file = "OG_artist_name_KB.RData")
