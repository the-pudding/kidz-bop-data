## double check

new = read.csv(file="moving_to_final/data/censorline-prelim-allFull.csv",stringsAsFactors =F) 

new = new %>% group_by(badword, kb_idx, og_artist, og_idx,song_name) %>%  mutate(count = sequence(n())) 


old=read.csv(file="moving_to_final/data/censorline-prelim-allAnnotatedFinal.csv", stringsAsFactors = F)

old = old %>% group_by(badword, kb_idx, og_artist, og_idx,song_name) %>%  mutate(count = sequence(n())) 

#df %>% group_by(IDFAM) %>% mutate(count = row_number(IDFAM))

dim(old)
dim(new)


names(old)
names(new)



#test=merge(old[,c("badword","kb_idx","og_artist","og_idx","song_name","jdCheck","forQuiz","noEdit","dropSong","replaceSong","count")], new, all.x=T, all.y=T, by.x=c("badword","kb_idx","og_artist","og_idx","song_name","count"), by.y=c("badword","kb_idx","og_artist","og_idx","song_name","count"))


test=merge(old[,c("badword","og_artist","song_name","jdCheck","forQuiz","noEdit","dropSong","replaceSong","count")], new, all.x=T, all.y=T, by.x=c("badword","og_artist","song_name","count"), by.y=c("badword","og_artist","song_name","count"))
#View(test)


test2=test[-which(test$dropSong=="Y"),]

#View(test2)

test3=test2[-which(test2$noEdit=="Y"),]

#test4=test3[-which(test3$replaceSong!=""),]

#names(test3)

use = names(old)
use = use[-8] ## data

test5=test4[,use]

test6=test5[c(which(test5$jdCheck=="" | is.na(test5$jdCheck)), setdiff(1:nrow(test5), which(test5$jdCheck=="" | is.na(test5$jdCheck)))),]



oldI = old %>% filter(noEdit!=T & dropSong!="Y") %>% group_by(og_artist, song_name) %>% summarise(count =n())

newI=test3 %>% group_by(og_artist, song_name) %>% summarise(count=n())


head(oldI)
head(newI)

View(old)

setdiff(oldI$og_artist, newI$og_artist) ## lost lil nas
setdiff( newI$og_artist, oldI$og_artist)

## some duplicates where one id is missing


final = test3 %>% group_by(og_artist, song_name, badword, count) %>% summarise(category=category[1], year=year[1], isCensored=isCensored[1], isPresent=isPresent[1], ogCase1=ogCase1[1], jdCheck=jdCheck[1], forQuiz=forQuiz[1])

newI = final %>% group_by(og_artist, song_name) %>% summarise(count=n())

oldI
newI

check = merge(oldI, newI, by.x=c("og_artist","song_name"), by.y=c("og_artist", "song_name"), all.x=T, all.y=T)

check[which(check$count.x!=check$count.y),] %>% View() ## ones with no edit so that seems good

write.csv(final, "moving_to_final/data/censorline-all.csv",row.names=F)

which(final$jdCheck=="" | is.na(final$jdCheck))


### NA in category don't exist so drop

final2=final[-which(is.na(final$category)),]

write.csv(final2, "moving_to_final/data/censorline-all.csv",row.names=F)

