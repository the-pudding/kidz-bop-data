setwd("~/Desktop/kidz-bop-data")

load(file = "data/2020-02-02/kbLyrics.RData")
load(file = "data/2020-02-02/ogLyricsFullPlain.RData")
load(file="data/missing/ogLyricsExtra.RData")
load(file="data/missing/kbLyricsExtraMatch.RData")
crosswalk = read.csv("data/2020-02-02/crosswalk.csv", stringsAsFactors = F)

library(dplyr)

helper <- function(x){
 # print(x)
  if(is.null(kbLyricsExtra[[x]]$result$line) | is.null(ogLyricsExtra[[x]]$result$line[1])){
    return(0)
  }else{
  kbW = strsplit(kbLyricsExtra[[x]]$result$line[1] , " ") %>% unlist()
  ogW = strsplit(ogLyricsExtra[[x]]$result$line[1] , " ") %>% unlist()
  
  return(length(intersect(kbW, ogW))/length(ogW))
  }

}

extraLineDiff = lapply(1:length(kbLyricsExtra), helper) %>% unlist() #%>% length()

helper <- function(x){
 #browser()
  if(is.null(ogLyricsFull[[crosswalk$og_idx[x]]]$line) | is.null(kbLyrics[[crosswalk$kb_idx[x]]]$line[1])){
    return(0)
  }else{
    ogW = strsplit(ogLyricsFull[[crosswalk$og_idx[x]]]$line[1] , " ") %>% unlist()
    kbW = strsplit(kbLyrics[[crosswalk$kb_idx[x]]]$line[1] , " ") %>% unlist()
    
    return(length(intersect(kbW, ogW))/length(ogW))
  }
  
}

lineDiff = lapply(1:nrow(crosswalk),helper) %>% unlist() #%>% length()

song_name = c(crosswalk$kb_song_name, song_name_extra = unlist(lapply(lapply(kbLyricsExtra, function(x){x$result$song_name[1]}), function(x){ifelse(is.null(x),NA, x)}) )) %>% unname()

propSameWordsLineOne = c( lineDiff, extraLineDiff)


write.csv(cbind.data.frame(song_name, propSameWordsLineOne), "data/censoring/firstLineComparison.csv", row.names=F)


ld = read.csv("data/censoring/firstLineComparison.csv", stringsAsFactors = F)  

ldN = read.csv("data/censoring/lineDifferencesNotes2.csv")

all = merge(ld, ldN, by.x="song_name", by.y="song_name", all.x=T, all.y=T)

write.csv(all, "data/censoring/lineDifferencesFirstLine.csv",row.names=F)

#test = read.csv("data/censoring/lineDifferencesFirstLine.csv", stringsAsFactors = F)
