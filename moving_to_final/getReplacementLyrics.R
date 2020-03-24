## get updated lyrics

toReplace = read.csv("moving_to_final/data/toReplace.csv", stringsAsFactors = F)


dim(toReplace)

songIds = c(551262, 415468, 2290025, 4133841, 533023, 193718)

library(geniusr)

genius_token() 

library(purrr)

safe_scrape <- safely(scrape_lyrics_id) ## make sure if we can't find a song's lyrics, it doesn't crash
kbLyrics <- map(songIds, ~ safe_scrape(.x)) ## patience, this takes awhile
save(kbLyrics, file = "moving_to_final/data/kbReplacementLyrics.RData")
