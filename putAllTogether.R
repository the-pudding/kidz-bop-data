extra1 <- read.csv("data/missing/extraC.csv", stringsAsFactors = F)
extra2 <- read.csv("data/missing/extraC2.csv", stringsAsFactors = F)
extra3 <- read.csv("data/missing/fullDataSet.csv", stringsAsFactors = F)

names(extra1)
names(extra2)
names(extra3)

names(extra1)[7:8]=c("og_release_year", "kb_release_year")
names(extra2)[9:10]=c("og_release_year", "kb_release_year")
names(extra3)[7]="song_name"

full = rbind(
extra1[,c("bad_word", "kbID", "bad_word_idx", "isStillPresent","isPresent","song_name", "og_release_year", "kb_release_year","isCensored","category")],
extra2[,c("bad_word", "kbID", "bad_word_idx", "isStillPresent","isPresent","song_name", "og_release_year", "kb_release_year","isCensored","category")],
extra3[,c("bad_word", "kbID", "bad_word_idx", "isStillPresent","isPresent","song_name", "og_release_year", "kb_release_year","isCensored","category")])
View(full)

full %>% group_by(song_name, kbID) %>% summarise() %>% nrow() ## 678

write.csv(full,"data/missing/fullCensor.csv",row.names = F)

extra3 %>% group_by(song_name, kbID)  %>% summarise() %>% nrow() ## 585
