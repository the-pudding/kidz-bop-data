setwd("~/Desktop/kidz-bop-data/")


setwd("~/Desktop/kidz-bop-data/")


badWords = read.csv("data/censoring/badWord_categories.csv", stringsAsFactors = F)

badWords = badWords[, c("bad_word", "category")] ## don't need preliminary counts
badWords = badWords[-1,] ## remove NA
#badWords = badWords[order(badWords$bad_word),]

censored = read.csv("data/censoring/fullDataSet.csv", stringsAsFactors = F)

inCensored = lapply(badWords$bad_word, function(x){grep(x, censored$censored_phrase)})

#censored[inCensored[[1]], ] %>% View()


eachSong = lapply(inCensored, function(x){censored[x, ] %>% group_by(kb_song_name, kb_release_year, og_release_year, og_artist) %>% summarise(count = n())})


badWords$numSongsCensored = unlist(lapply(eachSong, nrow))

badWords$firstTimeCensored = unlist(lapply(eachSong, function(x){min(x$kb_release_year)}))

load(file="data/2020-02-02/ogLyricsFullPlain.RData") ## ogLyricsFull

#summary(censored$og_idx)


og_songIDs = unique(censored$og_idx)


helper_findWordsOG <- function(ogIdx, badword_vctr){
  
  tryThis = ogLyricsFull[[ogIdx]]$line
  trythis = tolower(tryThis)
  
  tryThis = gsub("\\(", "", tryThis)
  tryThis = gsub("\\)", "", tryThis)
  ## take out punctuation
  tryThis = gsub("\\.", "", tryThis)
  tryThis = gsub("\\?", "", tryThis)
  tryThis = gsub(",", "", tryThis)
  tryThis = gsub("'", "", tryThis)
  tryThis = gsub("’", "", tryThis)
  # get rid of hyphens
  tryThis = gsub("-", "", tryThis)
  
  record = lapply(badword_vctr, function(x){length(grep(x,tryThis))})
  
  return(record)
}

look = lapply( og_songIDs,helper_findWordsOG, badWords$bad_word )

isWordIn = lapply(look, function(x){which(unlist(x)>0)}) ## 

match = cbind.data.frame(og_song = rep(og_songIDs, times = unlist(lapply(isWordIn,length))), bw = unlist(isWordIn))

#head(match)

match$badword = badWords$bad_word[match$bw]

toM = censored %>% group_by(kb_song_name, og_idx) %>% summarise()
match2 = merge(match, toM,by.x="og_song", by.y="og_idx",all.x=T, all.y=F)

byWord = split(match2, match2$badword)
#names(byWord)

#match2$badword %>% unique() %>% sort()

names(eachSong)=badWords$bad_word

helper_diffInCensor <- function(x){
  #browser()
  setdiff(byWord[[x]]$kb_song_name, eachSong[x]$kb_song_name)
}

not_censored = lapply(names(byWord), helper_diffInCensor )

censored = eachSong

length(not_censored)
length(censored)

