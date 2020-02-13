setwd("~/Desktop/kidz-bop-data")
censoredPhrases = read.csv("data/censoring/setdiffPhrasesPlus.csv",stringsAsFactors = F)

load(file="data/spotifyData.RData")

newKB = unique(censoredPhrases$kb_song_name)

oldKB = unique(all$track.name) %>% tolower()

#intersect(newKB, oldKB)

needDate = censoredPhrases$kb_song_name[which(is.na(censoredPhrases$kb_release))] %>% unique()

#intersect(needDate, oldKB) %>% length() ## 284
#length(needDate) ## 318

test = subset(censoredPhrases, is.na(censoredPhrases$kb_release))

test %>% group_by(kbID, kb_song_name) %>% summarise(count = n()) %>% group_by(kb_song_name) %>% summarise(total = n()) %>% arrange(desc(total)) ## ok, no doubles

multiples = all %>% group_by(track.name) %>% group_by(count=n()) %>% arrange(desc(count)) %>% select(track.name, count) 

solo = subset(multiples, count==1)
oldKB = unique(solo$track.name) %>% tolower()

intersect(needDate, oldKB) %>% length() ## 273
length(needDate) ## 318

## so I think this part is safe to merge

safe = subset(all, track.name %in% solo$track.name)
safe$track.name = tolower(safe$track.name)

tryThis = merge(censoredPhrases, safe[,c("track.album.release_date", "track.name", "track.album.name")],by.x="kb_song_name", by.y = "track.name",all.x=T)
View(tryThis)


tryThis[which(is.na(tryThis$track.album.release_date) & is.na(tryThis$kb_release)),"kb_song_name"] %>% unique() %>% length() ## 45

tryThis[which(is.na(tryThis$track.album.release_date) & is.na(tryThis$kb_release)),] %>% group_by(kb_song_name,kbID) %>% summarise(count = n()) %>% write.csv(., "data/2020-02-02/fillinKBdates.csv",row.names = F)

tryThis[which(is.na(tryThis$track.album.release_date) & is.na(tryThis$kb_release)),] %>% View()

tryThis %>% group_by(kb_song_name, og_artist) %>% summarise(count=n()) %>% View()

## kbIDs were unchanged so this is fine
toRemove = c(which(tryThis$kbID==211 & tryThis$og_artist !="Martin Solveig"),
which(tryThis$kbID==77 & tryThis$og_idx==70),

which(tryThis$kbID==295 & tryThis$og_artist !="Mario"),

which(tryThis$kbID==388 & tryThis$og_artist !="Ed Sheeran"),

which(tryThis$kbID==590 & tryThis$og_artist !="Chris Brown"),

which(tryThis$kbID==117)
)

toM = tryThis[-toRemove,]

#remove cool for now will need to replace later

## this stuff didn't change
extraDates = read.csv("data/2020-02-02/fillinKBdates-fillinKBdates.csv",stringsAsFactors = F)

test = merge(toM, extraDates[,c("kbID","release_date")], by.x="kbID", by.y="kbID",all.x=T)

which(is.na(test$kb_release) & is.na(test$track.album.release_date) & is.na(test$release_date)) ## woo


test$release_date_full = test$kb_release
test$release_date_full[is.na(test$release_date_full)]=test$track.album.release_date[is.na(test$release_date_full)]
test$release_date_full[is.na(test$release_date_full)]=test$release_date[is.na(test$release_date_full)]

sum(is.na(test$release_date_full))

head(test$release_date_full)


as.numeric(substr(test$release_date_full,1,4)) %>% summary()
why = as.numeric(substr(test$release_date_full,1,4))
test[which(why==1),]


test$release_date_full = substr(test$release_date_full,1,4)
#test[which(test$release_date_full=="0001"),]

# justin bieber's one time
test[which(test$release_date_full=="0001"),"release_date_full"]=2010

View(test)

names(test)

toSave = test[,c("kbID","og_idx","kb_song_name","og_artist","og_release","release_date_full","censored_phrase")] 

names(toSave)[6]="kb_release_year"

write.csv(toSave,"data/2020-02-02/almostthere.csv",row.names = F)


is.na(toSave$og_release) %>% sum()


toSave[is.na(toSave$og_release),] %>% group_by(kb_song_name, og_artist) %>% summarise(count =n()) %>% write.csv(.,"data/2020-02-02/fillinOGdates.csv",row.names = F)#%>% View()


### 	Doyle Bramhall II
### 482	459	take you there	Tyler Major

ogDates = read.csv("data/2020-02-02/fillinOGdates.csv", stringsAsFactors = F) ## fill in manually

toSave2 = merge(toSave, ogDates, by.x=c("kb_song_name","og_artist"), by.y = c("kb_song_name","og_artist"),all.x=T)

which(is.na(toSave2$og_release) & is.na(toSave2$year)) ## woo

toSave2$og_year = toSave2$og_release
toSave2$og_year[is.na(toSave2$og_year)] = toSave2$year[is.na(toSave2$og_year)]

sum(is.na(toSave2$og_year))

toSave2$og_year = substr(toSave2$og_year,1,4)

summary(as.numeric(toSave2$og_year))

names(toSave2)

toSave3 = toSave2[,c("kbID","og_idx","kb_song_name","og_artist","og_year","kb_release_year","censored_phrase")]

names(toSave3)[5]="og_release_year"

head(toSave3)


sum(is.na(toSave3$kbID))
sum(is.na(toSave3$og_idx))
sum(is.na(toSave3$kb_song_name))
sum(is.na(toSave3$og_artist))
sum(is.na(toSave3$censored_phrase)) ## drop these

toSaveForReal = toSave3[-which(is.na(toSave3$censored_phrase)),]

## found these need to drop
### 	Doyle Bramhall II
### 482	459	take you there	Tyler Major

toRemove = which(toSaveForReal$og_artist=="Doyle Bramhall II")
toRemove2 = which(toSaveForReal$og_artist=="Tyler Major")

toSaveForReal = toSaveForReal[-c(toRemove, toRemove2),]

write.csv(toSaveForReal,"data/censoring/fullDataSet.csv",row.names=F)
