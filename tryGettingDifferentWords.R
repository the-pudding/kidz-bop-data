setwd("~/Desktop/kidz-bop-data")

require(dplyr)

crosswalk = read.csv("data/2020-02-02/crosswalk.csv", stringsAsFactors = F)

load(file="data/2020-02-02/kbLyrics.RData") ## kbLyrics

load(file="data/2020-02-02/ogLyricsFullPlain.RData") ## ogLyricsFull

helper_diff = function(kb, og){
  kbWords = lapply(kbLyrics[[kb]]$line, strsplit," ")
  ogWords = lapply(ogLyricsFull[[og]]$line, strsplit, " ")
  
  kbWords = unique(tolower(unlist(kbWords)))
  ogWords = unique(tolower(unlist(ogWords)))
  
  ## strip out parentheses
  kbWords = gsub("\\(", "", kbWords)
  ogWords = gsub("\\(", "", ogWords)
  kbWords = gsub("\\)", "", kbWords)
  ogWords = gsub("\\)", "", ogWords)

  ## take out punctuation
  
  
  censored = setdiff(ogWords, kbWords) ## in original but not in kidz bop
  replacements = setdiff(kbWords, ogWords) ## in kidzbop but not in original
  
  return(list(censored=censored,replacements = replacements))
}

getDiff = mapply(helper_diff,crosswalk$kb_idx, crosswalk$og_idx, SIMPLIFY = F)


getDiff[[1]]
crosswalk[1,]
## gets at words but not phrases

#https://genius.com/Kidz-bop-kids-ready-for-it-lyrics
#https://genius.com/Taylor-swift-ready-for-it-lyrics

# phrases are going to mess this next part up but persevering for now

allCensored = lapply(getDiff, function(x){x$censored})

allCensoredTrack = cbind.data.frame(song = unlist(allCensored), kbID = rep(crosswalk$kb_idx, times = unlist(lapply(allCensored,length))))

helper_uncensored = function(kb, words){
 # browser()
  kbWords = lapply(kbLyrics[[kb]]$line, strsplit," ")
  
  kbWords = unique(tolower(unlist(kbWords)))

  ## strip out parentheses
  kbWords = gsub("\\(", "", kbWords)
  kbWords = gsub("\\)", "", kbWords)
  
  words = subset(words, kbID !=kb)
  
  return(intersect(kbWords, words$song))
}

# what has been censored before but isn't in others?

getChange = lapply(crosswalk$kb_idx, helper_uncensored, allCensoredTrack)

getChange[[1]] ## yeah, too many phrase changes, messing everything up

## if we can get a subset of words we actually want to go back and look for, this will work

tryThis = merge(allCensoredTrack,crosswalk,by.x="kbID", by.y="kb_idx",all.x=T)

write.csv(tryThis, "data/censoring/setdiffWords.csv",row.names = F)
