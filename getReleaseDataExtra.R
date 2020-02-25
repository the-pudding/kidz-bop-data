setwd("~/Desktop/kidz-bop-data")

load( file = "data/missing/ogLyricsExtra.RData") ## ogLyricsExtra
load(file="data/missing/kbLyricsExtraMatch.RData") ## kbLyricsExtra


kbLyricsExtra = lapply(kbLyricsExtra, function(x){x$result})
kbId = lapply(lapply(kbLyricsExtra, function(x){x$song_id[1]}) , function(x){ifelse(is.null(x),NA, x)}) %>% unlist() 
ogLyricsExtra = lapply(ogLyricsExtra, function(x){x$result})
ogId = lapply(lapply(ogLyricsExtra, function(x){x$song_id[1]}) , function(x){ifelse(is.null(x),NA, x)}) %>% unlist() 

safe_meta <- safely(get_song_meta) ## make sure if we can't find a song, it doesn't crash
ogDetailsExtra<- map(ogId, ~ safe_meta(.x))
save(ogDetailsExtra, file="data/missing/ogDetailsRawExtra.RData")
kbDetailsExtra<- map(kbId, ~ safe_meta(.x))
save(kbDetailsExtra, file="data/missing/kbDetailsRawExtra.RData")


ogde=do.call("rbind",lapply(ogDetailsExtra, function(x){x$result}))
kbde=do.call("rbind",lapply(kbDetailsExtra, function(x){x$result}))

ogInfo = cbind.data.frame(og_song_id = ogId, extraID = 1:length(ogId))
kbInfo = cbind.data.frame(kb_song_id = kbId,  extraID = 1:length(kbId))

ogInfo$og_song_id= as.character(ogInfo$og_song_id)
ogInfo$extraID= as.character(ogInfo$extraID)

kbInfo$kb_song_id= as.character(kbInfo$kb_song_id)
kbInfo$extraID= as.character(kbInfo$extraID)

all = merge(ogInfo, kbInfo, by.x="extraID", by.y="extraID",all.x=T,all.y=T)

all2= merge(all,ogde[,c("song_id", "song_name", "release_date")],by.x="og_song_id", by.y="song_id",all.x=T)
names(all2)[5]="og_release_date"

all3= merge(all2, kbde[,c("song_id", "release_date")], by.x="kb_song_id", by.y="song_id",all.x=T)

names(all3)[6]="kb_release_date"

View(all3)
library(lubridate)
all3$ogRelease = year(as.Date(all3$og_release_date))
all3$kbRelease = year(as.Date(all3$kb_release_date))

names(all3)[3]="storageId"

write.csv(all3, "data/missing/extraDates.csv",row.names=F) ## fill in some missing

## remove banana brain and go christmas

extraHave = read.csv("data/missing/newHaveLyricsAlready.csv", stringsAsFactors = F) ## so few, just do manually


