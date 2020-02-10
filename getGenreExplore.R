setwd("/Users/Sara/Desktop/kidz-bop-data")
require(dplyr)

artists <- read.csv("data/2020-02-02/originalArtists.csv",stringsAsFactors = F)

head(artists)

library(spotifyr)

spotify_client_id <- "" ## put yours here
spotify_client_secret <- "" ## put yours here
access_token <- get_spotify_access_token(client_id = spotify_client_id, client_secret = spotify_client_secret)

Sys.setenv(SPOTIFY_CLIENT_ID = spotify_client_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = spotify_client_secret)

## genre is by album/artist

test = get_discography(artists$og_artist[1]) ## need to get full discography just to get artist id, will need to manually enforce rate limit
names(test)
#grep("genre",names(test))

tryThis = get_artists(test$artist_id[1]) ## vectorized at least


 