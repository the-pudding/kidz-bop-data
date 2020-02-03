require(dplyr)
setwd("~/Desktop/kidz-bop-data")

kbNames = read.csv("data/2020-02-02/kbSongNames.csv", stringsAsFactors = F)
ogNames = read.csv("data/2020-02-02/ogSongNamesMore.csv", stringsAsFactors = F)
names(kbNames)[1]="kb_idx"
names(ogNames)[1]="og_idx"

kbNames$kb_song_name = tolower(kbNames$kb_song_name)
ogNames$og_song_name = tolower(ogNames$og_song_name)

all = merge(kbNames, ogNames, by.x= "kb_song_name", by.y = "og_song_name" ,all.x=T, all.y=T)

View(all)

which(!is.na(all$kb_idx) & !is.na(all$og_idx)) %>% length() ## 618

which(is.na(all$kb_idx) & !is.na(all$og_idx)) %>% length() ## 102 (don't need to worry about these right now)

nrow(all) - 618 - 102 ## none to fix manually, woah

write.csv(all,file="data/2020-02-02/crosswalk.csv",row.names=F)
