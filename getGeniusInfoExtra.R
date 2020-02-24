### get new genius

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


# ditch first one anyway
# replace birthday with https://genius.com/The-beatles-birthday-lyrics
# replace down with https://genius.com/Marian-hill-down-lyrics
# replace cool with https://genius.com/Gwen-stefani-cool-lyrics
# fix i got a feeling https://genius.com/The-black-eyed-peas-i-gotta-feeling-lyrics
# fix mickey https://genius.com/Toni-basil-mickey-12-version-lyrics
# over you https://genius.com/Daughtry-over-you-lyrics
# paradise https://genius.com/George-ezra-paradise-lyrics
# two nothing breaks like a heart?
# wild ones https://genius.com/Flo-rida-wild-ones-lyrics
# it's your birthday, kidz bop shuffle, life of the party, sounds like a hit, is kb og
## ugh christmas, pb jelly time