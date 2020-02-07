setwd("~/Desktop/kidz-bop-data/data/censoring/lyric_lines")

test = read.delim("diff550.txt",header = F)

#https://stackoverflow.com/questions/16118050/how-to-check-if-a-vector-contains-n-consecutive-numbers
seqle <- function(x,incr=1) { 
  if(!is.numeric(x)) x <- as.numeric(x) 
  n <- length(x)  
  y <- x[-1L] != x[-n] + incr 
  i <- c(which(y|is.na(y)),n) 
  list(lengths = diff(c(0L,i)),
       values = x[head(c(0L,i)+1L,-1L)]) 
} 

phrase_helper <- function(seqINFO, lyricBlock){
  #browser()
  start = seqINFO$values[which(seqINFO$lengths>=1)]
  end = seqINFO$values[which(seqINFO$lengths>=1)]+seqINFO$lengths[which(seqINFO$lengths>=1)]-1
  
  toReturn =  lapply(mapply(function(x,y){lyricBlock[x:y]},start, end, SIMPLIFY = F), function(x){paste(as.character(x),collapse = " ")}) %>% unlist()
  
  toReturn = gsub("-","",toReturn)
  
  return(toReturn)
  
}

getCensoredPhrases <- function(fileName){
  test = read.delim(fileName,header = F)
  test = test[-c(1,2),]
  #https://stackoverflow.com/questions/25411653/how-do-i-split-a-vector-into-a-list-of-vectors-when-a-condition-is-met/25411832
  tryThis = split(test[,1], cumsum(1:length(test[,1]) %in% grep("@", test[,1])))
  
  tryThis = lapply(tryThis, function(x){x[-1]})
  
  tryThis2 = lapply(tryThis, function(x){as.character(x)})
  
  minus = lapply(tryThis2, function(x){grep("-", x)})
  
  seqInfo = lapply(minus,seqle)
#browser()

  censored = mapply(phrase_helper, seqInfo, tryThis2, SIMPLIFY = F)
  
return(censored)
  
}

testitout=getCensoredPhrases("diff550.txt") 

testitout2=getCensoredPhrases("diff401.txt")  ## works for one word now, woo

## the empty one is addition only, deal with later

diffFiles = grep("diff",list.files(),value=T)
diffFiles = diffFiles[-length(diffFiles)] ## NA

diffFiles2= gsub("diff","",diffFiles)
diffFiles2= gsub(".txt","",diffFiles2)

#cp = lapply(diffFiles,getCensoredPhrases)

cp = vector("list",length(diffFiles))
for(i in 1:length(diffFiles)){
  
    cp[[i]]=tryCatch(getCensoredPhrases(diffFiles[i]),error=function(e){NA})
  
    cp[[i]][unname(which(unlist(lapply(cp[[i]],length))==0))]=NA

  print(i)
}



which(is.na(cp)) %>% length() ## 13, ok fine, figure out later

isthisright=cp[-which(is.na(cp))]


allCensoredTrack = cbind.data.frame(song = unname(unlist(isthisright)), kbID = rep(diffFiles2[-which(is.na(cp))], times = unlist(lapply(isthisright, function(x){length(unlist(x))}))))


tryThis = merge(allCensoredTrack,crosswalk,by.x="kbID", by.y="kb_idx",all.x=T)

setwd("~/Desktop/kidz-bop-data")
write.csv(tryThis, "data/censoring/setdiffPhrases.csv",row.names = F)
## woo down to 6469
