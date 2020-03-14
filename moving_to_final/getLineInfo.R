setwd("~/Desktop/kidz-bop-data/")

censorD= read.csv( "moving_to_final/data/proportions-kb-prep.csv", stringsAsFactors = F)
censorD = censorD[,-which(names(censorD)%in% c("year.x", "year.y"))]

censorD$isCensored = ifelse(censorD$numOccurKB < censorD$numOccurOG, 1, 0)
censorD$isPresent = ifelse(censorD$numOccurOG>0, 1, 0)

censorD$category = ifelse(censorD$category %in% c("profanity","slur"), "profanity", censorD$category)
censorD$category = ifelse(censorD$category %in% c("gender & sexuality"), "identity", censorD$category)
censorD$category = ifelse(censorD$category %in% c("gender & sexuality"), "identity", censorD$category)
censorD$category = ifelse(censorD$category %in% c("religious","mental health","other"), "other", censorD$category)



censorInfo = subset(censorD, isCensored == 1)


helper <- function(idx){
  bad_word = censorInfo$anchored[idx]
  og_idx = censorInfo$og_idx[idx]
  kb_idx = censorInfo$kb_idx[idx]
  dataset = censorInfo$data[idx]
  
  getLine(bad_word, og_idx, kb_idx, dataset)
  
}

getLine<- function(bad_word, og_idx, kb_idx, dataset){
  #browser()
  if(dataset=="first"){
    ogL = tolower(ogLyricsFull[[og_idx]]$line)
    kbL = kbLyrics[[kb_idx]]$line

  test =  lapply(lapply(strsplit(ogL, " "), function(x){grep(bad_word,x)}), function(x){ifelse(length(x)==0,NA, x)}) %>% unlist()
  
  ogCaseID = which(!is.na(test))
   ogCase = ogLyricsFull[[og_idx]]$line[ogCaseID]               
   kbReplace = kbL[ogCaseID]
  }else{
    ogL = tolower(ogLyricsExtra[[og_idx]]$result$line)
    kbL = kbLyricsExtra[[kb_idx]]$result$line
    test =  lapply(lapply(strsplit(ogL, " "), function(x){grep(bad_word,x)}), function(x){ifelse(length(x)==0,NA, x)}) %>% unlist()
    ogCaseID = which(!is.na(test))
    ogCase = ogLyricsExtra[[og_idx]]$result$line[ogCaseID]     
    kbReplace = kbL[ogCase]
  }
  return(list(ogCase = ogCase, kbReplace = kbReplace))
}

load(file = "data/2020-02-02/kbLyrics.RData")
load(file = "data/2020-02-02/ogLyricsFullPlain.RData")
load(file="data/missing/ogLyricsExtra.RData")
load(file="data/missing/kbLyricsExtraMatch.RData")
crosswalk = read.csv("data/2020-02-02/crosswalk.csv", stringsAsFactors = F)


lineData = lapply(1:nrow(censorInfo), helper)

length(lineData)
nrow(censorInfo)

ogCase = lapply(lineData, function(x){x$ogCase})

summary(unlist(lapply(ogCase, length)))

kbReplace = lapply(lineData, function(x){x$kbReplace})

summary(unlist(lapply(kbReplace, length)))




ogCase1 = lapply(ogCase, function(x){x[1]}) %>% unlist()
kbReplace1 = lapply(lapply(kbReplace, function(x){x[1]}), function(x){ifelse(is.null(x),NA, x)}) %>% unlist()



censorInfo$ogCase1 = ogCase1
censorInfo$kbReplace1=kbReplace1

df <- apply(censorInfo,2,as.character)


write.csv(df, file="moving_to_final/data/censorline-prelim.csv",row.names=F)        
