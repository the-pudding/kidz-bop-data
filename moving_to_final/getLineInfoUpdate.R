setwd("~/Desktop/kidz-bop-data/")
library(dplyr)
addon = read.csv("moving_to_final/data/proportions-kb-prepAddOn.csv", stringsAsFactors = F)
addon = addon[,-2] ## minus id
censorD = addon

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

load(file="moving_to_final/data/kbReplacementLyrics.RData") ## kbLyrics

ogUpdate = kbLyrics
load(file = "data/2020-02-02/kbLyrics.RData")

getLine<- function(bad_word, og_idx, kb_idx, dataset){
  # browser()

    ogL = tolower(ogUpdate[[og_idx]]$line)
    ogL = gsub("\\(", "", ogL)
    ogL = gsub("\\)", "", ogL)
    ## take out punctuation
    ogL = gsub("\\.", "", ogL)
    ogL = gsub("\\?", "", ogL)
    ogL = gsub("\\!", "", ogL)
    ogL = gsub(",", "", ogL)
    ogL = gsub("'", "", ogL)
    ogL = gsub("’", "", ogL)
    # get rid of hyphens
    ogL = gsub("-", "", ogL)
    kbL = kbLyrics[[kb_idx]]$line
    
    test =  lapply(lapply(strsplit(ogL, " "), function(x){grep(bad_word,x)}), function(x){ifelse(length(x)==0,NA, x)}) %>% unlist()
    
    ogCaseID = which(!is.na(test))
    ogCase = ogUpdate[[og_idx]]$line[ogCaseID]               
    kbReplace = kbL[ogCaseID]
  
  return(list(ogCase = ogCase, kbReplace = kbReplace, ogLines = length(ogL), kbLines = length(kbL)))
}


helper(24)

lineData = lapply(1:nrow(censorInfo), helper)

length(lineData)
nrow(censorInfo)

ogCase = lapply(lineData, function(x){x$ogCase})

summary(unlist(lapply(ogCase, length)))

kbReplace = lapply(lineData, function(x){x$kbReplace})

summary(unlist(lapply(kbReplace, length)))

ogLines = lapply(lineData, function(x){x$ogLines}) %>% unlist()
kbLines = lapply(lineData, function(x){x$kbLines}) %>% unlist()

summary(ogLines- kbLines)
which(abs(ogLines - kbLines)<5) 
which(abs(ogLines - kbLines)==0 ) %>% length() 



#length(unlist(ogLines) ) == nrow(censorInfo)
#length(unlist(kbLines) ) == nrow(censorInfo)


ogCase1 = lapply(ogCase, function(x){x[1]}) %>% unlist()
kbReplace1 = lapply(lapply(kbReplace, function(x){x[1]}), function(x){ifelse(is.null(x),NA, x)}) %>% unlist()


censorInfo$ogCase1 = ogCase1
censorInfo$kbReplace1=kbReplace1



## same number of lines?


## additional occurrences

helper_extra <- function(ogCase, kbReplace,id){
  if(length(ogCase)>1){
    ogR=ogCase[2:length(ogCase)]
    kbR = kbReplace[2:length(kbReplace)]
    kbR = c(kbR, rep(NA, length(ogR)-length(kbR)))
    return(cbind.data.frame(censorInfo[id,1:13],ogCase1=ogR, kbReplace1=kbR))
  }else{
    return(NULL) 
  }
}

testthis=mapply(helper_extra, ogCase, kbReplace, 1:length(ogCase), SIMPLIFY = F)

test=do.call("rbind",testthis)


full = rbind.data.frame(censorInfo,test)

write.csv(full, file="moving_to_final/data/censorline-prelim-allF.csv",row.names=F) 
