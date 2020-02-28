setwd("~/Desktop/kidz-bop-data/")


full = read.csv("data/missing/fullCensorTest.csv", stringsAsFactors = F)

head(full)

probablyWrong = c(
which(full$bad_word=="fuck"& full$isStillPresent ),
which(full$bad_word=="nigga"& full$isStillPresent ))


full[probablyWrong,]
#https://genius.com/Kidz-bop-kids-fallin-lyrics ## not even the right song
#https://genius.com/Kidz-bop-kids-suit-and-tie-lyrics 
#https://genius.com/Kidz-bop-kids-u-dont-have-to-call-lyrics
#https://genius.com/Kidz-bop-kids-dilemma-lyrics

weird = full[which(full$isStillPresent & !full$isPresent),] 
unique(weird$song_name) %>% length()

## you've got a friend is actually this https://genius.com/Carole-king-youve-got-a-friend-lyrics
## best time ever not best song ever

load("../2020-02-02/kbLyrics.RData")

kbLyrics[[60]]

load("../2020-02-02/ogLyricsFullPlain.RData")

ogLyricsFull[[219]]


bad = read.csv("../censoring/badWord_categories.csv", stringsAsFactors = F)
head(bad)
View(bad)



wrong just got paid

don't stop the party is wrong song'

https://genius.com/Kidz-bop-kids-shape-of-my-heart-lyrics 
wrong shape of my heart