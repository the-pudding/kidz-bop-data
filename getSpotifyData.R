library(spotifyr)
library(dplyr)

spotify_client_id <- "" ## put yours here
spotify_client_secret <- "" ## put yours here
access_token <- get_spotify_access_token(client_id = spotify_client_id, client_secret = spotify_client_secret)

Sys.setenv(SPOTIFY_CLIENT_ID = spotify_client_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = spotify_client_secret)


kb1_37_1 <- get_playlist_tracks("0YpfM2KShDPGhsTnj9gLxm", authorization = access_token)

kb1_37_2 <- get_playlist_tracks("0YpfM2KShDPGhsTnj9gLxm", authorization = access_token, offset=100) ## limit of 100 per grab


kb1_37_3 <- get_playlist_tracks("0YpfM2KShDPGhsTnj9gLxm", authorization = access_token, offset=200)


kb1_37_4 <- get_playlist_tracks("0YpfM2KShDPGhsTnj9gLxm", authorization = access_token, offset=300)


kb1_37_5 <- get_playlist_tracks("0YpfM2KShDPGhsTnj9gLxm", authorization = access_token, offset=400)

kb1_37_6 <- get_playlist_tracks("0YpfM2KShDPGhsTnj9gLxm", authorization = access_token, offset=500)

kb1_37_7 <- get_playlist_tracks("0YpfM2KShDPGhsTnj9gLxm", authorization = access_token, offset=600) 

kb37_40 <- get_playlist_tracks("4P7PbouN3FQP7yrXXqWjQq", authorization = access_token)

all = rbind.data.frame(kb1_37_1, kb1_37_2, kb1_37_3, kb1_37_4, kb1_37_5, kb1_37_6, kb1_37_7, kb37_40)

# unique(all$track.album.name) 

# remove duplicated 37
all = all[-which(duplicated(all$track.id)),]

# check, yes 40 actually is really large
all %>% group_by(track.album.name) %>% summarise(count=n()) %>% as.data.frame()


baseDir = "~/Desktop/" ## change accordingly

setwd(paste0(baseDir,"kidz-bop-data"))
save(all, file="data/spotifyData.RData")

#track.album.name
#track.name
#track.album.release_date

# need to get genre with a different function
