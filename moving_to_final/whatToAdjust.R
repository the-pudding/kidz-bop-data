library(dplyr)

setwd("~/Desktop/kidz-bop-data/")

dataA = read.csv("moving_to_final/data/censorline-prelim-allAnnotatedFinal.csv", stringsAsFactors = F)

#View(dataA)

#names(dataA)

#unique(dataA$dropSong)

toRemove = dataA[which(dataA$dropSong == "Y"),]


toRemove = toRemove %>% group_by(kb_idx, og_idx, data, song_name, og_artist) %>% summarise(count = n())

notC = dataA[which(dataA$noEdit == "Y"),]


notC = notC %>% group_by(kb_idx, og_idx, badword, song_name, og_artist) %>% summarise(count=n())

toReplace = dataA[which(dataA$replaceSong != ""),]

toReplace = toReplace %>% group_by(kb_idx, og_idx, replaceSong, data) %>% summarise(count = n())

write.csv(toRemove,"moving_to_final/data/toRemove.csv", row.names = F)
write.csv(toReplace,"moving_to_final/data/toReplace.csv", row.names = F)
write.csv(notC,"moving_to_final/data/notC.csv", row.names = F)

