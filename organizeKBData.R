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
  grep(paste0("^",pattern,"$"), kbSongs$song_name)[1]
}

tryThis = lapply(all$track.name,helper)

#cbind.data.frame(all$track.name,kbSongs$song_name[unlist(tryThis)]) %>% View()

geniusMatch = cbind.data.frame(all,geniusSongName = kbSongs$song_name[unlist(tryThis)], geniusIdx = unlist(tryThis)) 

save(geniusMatch, file="data/kbMatch.RData")

geniusMatch[,c("geniusSongName","track.name", "geniusIdx")] %>% View()

sum(!is.na(geniusMatch$geniusIdx))
dim(geniusMatch)

## two "fly"
## two "The Middle"
## two "Senorita"

grep("Fly", all$track.name)
grep("Fly", kbSongs$song_name)

kbLyrics[[212]] ## Sugar Ray

all[c(17, 360), ] %>% View()
## second one is Nicki Minaj

geniusMatch$track.name[360]=NA
geniusMatch$geniusIdx[360]=NA

grep("The Middle", all$track.name)
grep("The Middle", kbSongs$song_name)

kbLyrics[[598]] ## Maren Morris

all[c(72, 634), ] %>% View()
# first one is Jimmy Eat World

geniusMatch$track.name[72]=NA
geniusMatch$geniusIdx[72]=NA

grep("rita", all$track.name)
grep("rita", kbSongs$song_name)

kbLyrics[[517]]  ## Shawn Mendes

all[c(84, 648), ] %>% View()
# first one is Justin Timberlake

geniusMatch$track.name[84]=NA
geniusMatch$geniusIdx[84]=NA


save(geniusMatch, file="data/kbMatch.RData")

## have to deal with these
sum(!is.na(geniusMatch$geniusIdx))

which(table(geniusMatch$geniusIdx)>1)

kbSongs[which(table(geniusMatch$geniusIdx)>1),"song_name"]

geniusMatch[which(geniusMatch$geniusIdx==51),]

grep("Bad Liar", all$track.name)
grep("Bad Liar", kbSongs$song_name)

kbLyrics[[51]] 

all[c(593, 679), ] %>% View()
# second one is the match

geniusMatch$track.name[593]=NA
geniusMatch$geniusIdx[593]=NA

which(table(geniusMatch$geniusIdx)>1)

geniusMatch[which(geniusMatch$geniusIdx==56),]

grep("^Beautiful$", all$track.name)
grep("^Beautiful$", kbSongs$song_name)

kbLyrics[[56]] 

all[c(59, 620), ] %>% View()
# first one is the match

geniusMatch$track.name[620]=NA
geniusMatch$geniusIdx[620]=NA

which(table(geniusMatch$geniusIdx)>1)

geniusMatch[which(geniusMatch$geniusIdx==91),]

grep("^Boyfriend$", all$track.name)
grep("^Boyfriend$", kbSongs$song_name)

kbLyrics[[91]] 

all[c(158, 380), ] %>% View()
# second one is the match

geniusMatch$track.name[158]=NA
geniusMatch$geniusIdx[158]=NA

which(table(geniusMatch$geniusIdx)>1)

geniusMatch[which(geniusMatch$geniusIdx==105),]

grep("^Burn$", all$track.name)
grep("^Burn$", kbSongs$song_name)

kbLyrics[[105]] 

all[c(103, 433), ] %>% View()
# neither one matches

geniusMatch$track.name[103]=NA
geniusMatch$geniusIdx[103]=NA

geniusMatch$track.name[433]=NA
geniusMatch$geniusIdx[433]=NA

which(table(geniusMatch$geniusIdx)>1)

geniusMatch[which(geniusMatch$geniusIdx==141),]


grep("^Cool$", all$track.name)
grep("^Cool$", kbSongs$song_name)

kbLyrics[[141]] 

all[c(152, 664), ] %>% View()
# first one is match

geniusMatch$track.name[664]=NA
geniusMatch$geniusIdx[664]=NA


which(table(geniusMatch$geniusIdx)>1)

geniusMatch[which(geniusMatch$geniusIdx==227),]


grep("^Girlfriend$", all$track.name)
grep("^Girlfriend$", kbSongs$song_name)

kbLyrics[[227]] 

all[c(49, 200), ] %>% View()
# second one is match

geniusMatch$track.name[49]=NA
geniusMatch$geniusIdx[49]=NA


which(table(geniusMatch$geniusIdx)>1)

geniusMatch[which(geniusMatch$geniusIdx==249),]


grep("^Happy$", all$track.name)
grep("^Happy$", kbSongs$song_name)

kbLyrics[[249]] 

all[c(47, 436), ] %>% View()
# second one is match

geniusMatch$track.name[47]=NA
geniusMatch$geniusIdx[47]=NA


which(table(geniusMatch$geniusIdx)>1)

geniusMatch[which(geniusMatch$geniusIdx==259),]


grep("^Hello$", all$track.name)
grep("^Hello$", kbSongs$song_name)

kbLyrics[[259]] 

all[c(361, 523), ] %>% View()
# first one is match

geniusMatch$track.name[523]=NA
geniusMatch$geniusIdx[523]=NA


which(table(geniusMatch$geniusIdx)>1)

geniusMatch[which(geniusMatch$geniusIdx==267),]


grep("^Hero$", all$track.name)
grep("^Hero$", kbSongs$song_name)

kbLyrics[[267]] 

all[c(30, 51), ] %>% View()
# first one is match

geniusMatch$track.name[51]=NA
geniusMatch$geniusIdx[51]=NA


which(table(geniusMatch$geniusIdx)>1)


geniusMatch[which(geniusMatch$geniusIdx==280),]


grep("^Home$", all$track.name)
grep("^Home$", kbSongs$song_name)

kbLyrics[[280]] 

all[c(217, 399), ] %>% View()
# first one is match

geniusMatch$track.name[399]=NA
geniusMatch$geniusIdx[399]=NA


which(table(geniusMatch$geniusIdx)>1)

geniusMatch[which(geniusMatch$geniusIdx==365),]


grep("^Let Me Go$", all$track.name)
grep("^Let Me Go$", kbSongs$song_name)

kbLyrics[[365]] 

all[c(142, 637), ] %>% View()
# second one is match

geniusMatch$track.name[142]=NA
geniusMatch$geniusIdx[142]=NA


which(table(geniusMatch$geniusIdx)>1)

geniusMatch[which(geniusMatch$geniusIdx==366),]


grep("^Let Me Love You$", all$track.name)
grep("^Let Me Love You$", kbSongs$song_name)

kbLyrics[[366]] 

all[c(133, 396, 555), ] %>% View()
# first one is match

geniusMatch$track.name[396]=NA
geniusMatch$geniusIdx[396]=NA


geniusMatch$track.name[555]=NA
geniusMatch$geniusIdx[555]=NA

which(table(geniusMatch$geniusIdx)>1)

geniusMatch[which(geniusMatch$geniusIdx==429),]


grep("^Never Say Never$", all$track.name)
grep("^Never Say Never$", kbSongs$song_name)

kbLyrics[[429]] 

all[c(300, 350), ] %>% View()
# first one is match

geniusMatch$track.name[350]=NA
geniusMatch$geniusIdx[350]=NA

which(table(geniusMatch$geniusIdx)>1)

geniusMatch[which(geniusMatch$geniusIdx==471),]


grep("^Photograph$", all$track.name)
grep("^Photograph$", kbSongs$song_name)

kbLyrics[[471]] 

all[c(153, 504), ] %>% View()
# second one is match

geniusMatch$track.name[153]=NA
geniusMatch$geniusIdx[153]=NA

which(table(geniusMatch$geniusIdx)>1)

geniusMatch[which(geniusMatch$geniusIdx==514),]


grep("^See You Again$", all$track.name)
grep("^See You Again$", kbSongs$song_name)

kbLyrics[[514]] 

all[c(239, 493), ] %>% View()
# second one is match

geniusMatch$track.name[239]=NA
geniusMatch$geniusIdx[239]=NA

which(table(geniusMatch$geniusIdx)>1)

geniusMatch[which(geniusMatch$geniusIdx==542),]


grep("^Sorry$", all$track.name)
grep("^Sorry$", kbSongs$song_name)

kbLyrics[[542]] 

all[c(249, 516), ] %>% View()
# second one is match

geniusMatch$track.name[249]=NA
geniusMatch$geniusIdx[249]=NA

which(table(geniusMatch$geniusIdx)>1)

geniusMatch[which(geniusMatch$geniusIdx==544),]


grep("^SOS$", all$track.name)
grep("^SOS$", kbSongs$song_name)

kbLyrics[[544]] 

all[c(163, 233), ] %>% View()
# first one is match

geniusMatch$track.name[233]=NA
geniusMatch$geniusIdx[233]=NA

which(table(geniusMatch$geniusIdx)>1)

geniusMatch[which(geniusMatch$geniusIdx==703),]


grep("^With You$", all$track.name)
grep("^With You$", kbSongs$song_name)

kbLyrics[[703]] 

all[c(91, 252), ] %>% View()
# second one is match

geniusMatch$track.name[91]=NA
geniusMatch$geniusIdx[91]=NA

which(table(geniusMatch$geniusIdx)>1)
save(geniusMatch, file="data/kbMatch.RData")

sum(!is.na(geniusMatch$geniusIdx))
