setwd("~/Desktop/kidz-bop-data")

load(file = "data/2020-02-02/kbLyrics.RData")
load(file = "data/2020-02-02/ogLyricsFullPlain.RData")
load(file="data/missing/ogLyricsExtra.RData")
load(file="data/missing/kbLyricsExtraMatch.RData")
crosswalk = read.csv("data/2020-02-02/crosswalk.csv", stringsAsFactors = F)

library(dplyr)

(kbLyricsExtra[[1]]$result$line == ogLyricsExtra[[1]]$result$line ) %>% sum()

extraLineDiff = lapply(1:length(kbLyricsExtra), function(x){sum(kbLyricsExtra[[x]]$result$line == ogLyricsExtra[[x]]$result$line )}) %>% unlist() #%>% length()


lineDiff = lapply(1:nrow(crosswalk),function(x){sum(ogLyricsFull[[crosswalk$og_idx[x]]]$line == kbLyrics[[crosswalk$kb_idx[x]]]$line)}) %>% unlist() #%>% length()


song_name = c(crosswalk$kb_song_name, song_name_extra = unlist(lapply(lapply(kbLyricsExtra, function(x){x$result$song_name[1]}), function(x){ifelse(is.null(x),NA, x)}) )) %>% unname()

lineDiffI = c(extraLineDiff, lineDiff)

write.csv(cbind.data.frame(song_name, lineDiffI), "data/censoring/numberOfLinesDifferent.csv", row.names=F)


ld = read.csv("data/censoring/numberOfLinesDifferent.csv", stringsAsFactors = F)  

ldN = read.csv("data/censoring/lineDifferencesNotes.csv")

head(ld)
head(ldN)

all = merge(ld, ldN, by.x="song_name", by.y="song_name", all.x=T, all.y=T)
View(all)
