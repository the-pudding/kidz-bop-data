## double check

new = read.csv(file="moving_to_final/data/censorline-prelim-allFull.csv",stringsAsFactors =F) 

new = new %>% group_by(badword, kb_idx, og_artist, og_idx,song_name) %>%  mutate(count = sequence(n())) 


old=read.csv(file="moving_to_final/data/censorline-prelim-allAnnotatedFinal.csv", stringsAsFactors = F)

old = old %>% group_by(badword, kb_idx, og_artist, og_idx,song_name) %>%  mutate(count = sequence(n())) 

df %>% group_by(IDFAM) %>% mutate(count = row_number(IDFAM))

dim(old)
dim(new)


names(old)
names(new)




test=merge(old[,c("badword","kb_idx","og_artist","og_idx","song_name","jdCheck","forQuiz","noEdit","dropSong","replaceSong")], new, all.x=T, all.y=T, by.x=c("badword","kb_idx","og_artist","og_idx","song_name"), by.y=c("badword","kb_idx","og_artist","og_idx","song_name"))

test=merge(old[,c("badword","kb_idx","og_artist","og_idx","song_name","jdCheck","forQuiz","noEdit","dropSong","replaceSong","count")], new, all.x=F, all.y=T, by.x=c("badword","kb_idx","og_artist","og_idx","song_name","count"), by.y=c("badword","kb_idx","og_artist","og_idx","song_name","count"))

View(test)
