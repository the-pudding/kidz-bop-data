setwd("~/Desktop/kidz-bop-data")

require(dplyr)

crosswalk = read.csv("data/2020-02-02/crosswalk.csv", stringsAsFactors = F)

load(file="data/2020-02-02/kbLyrics.RData") ## kbLyrics

load(file="data/2020-02-02/ogLyricsFullPlain.RData") ## ogLyricsFull

setwd("~/Desktop/kidz-bop-data/data/censoring/lyric_lines")

helper_writeOG <- function(x){
  fileConn<-file(paste0("og",x,".txt"))
  if(!is.null(ogLyricsFull[[x]]$line)){
    tryThis = lapply(ogLyricsFull[[x]]$line,strsplit," ")
    
    # lower
    tryThis = unique(tolower(unlist(tryThis)))
    ## strip out parentheses
    tryThis = gsub("\\(", "", tryThis)
    tryThis = gsub("\\)", "", tryThis)
    ## take out punctuation
    tryThis = gsub("\\.", "", tryThis)
    tryThis = gsub("\\?", "", tryThis)
    tryThis = gsub(",", "", tryThis)
    tryThis = gsub("'", "", tryThis)

    
    
    fileConn<-file(paste0("og",x,".txt"))
    writeLines(tryThis, fileConn)
    close(fileConn)
  }else{
    print(x)
  }
  
}

list.files() %>% length()
nrow(crosswalk) ## 7 missing

lapply(crosswalk$og_idx, helper_writeOG)

helper_writeKB <- function(x){
  if(!is.null(kbLyrics[[x]]$line)){
    tryThis = lapply(kbLyrics[[x]]$line,strsplit," ")
    
    # lower
    tryThis = unique(tolower(unlist(tryThis)))
    ## strip out parentheses
    tryThis = gsub("\\(", "", tryThis)
    tryThis = gsub("\\)", "", tryThis)
    ## take out punctuation
    tryThis = gsub("\\.", "", tryThis)
    tryThis = gsub("\\?", "", tryThis)
    tryThis = gsub(",", "", tryThis)
    tryThis = gsub("'", "", tryThis)
    
    
    fileConn<-file(paste0("kb",x,".txt"))
    writeLines(tryThis, fileConn)
    close(fileConn)
  }else{
    print(x)
  }
  
}

lapply(crosswalk$kb_idx, helper_writeKB)

list.files() %>% length()
nrow(crosswalk)*2 ##  124 


helper_getDiffBash <- function(ogIdx,kbIdx){
  cmd = paste0("diff -u og",ogIdx,".txt kb",kbIdx,".txt > diff",kbIdx,".txt")
  system(cmd)
}

helper_getDiffBash(crosswalk$og_idx[1], crosswalk$kb_idx[1])

mapply(helper_getDiffBash, crosswalk$og_idx, crosswalk$kb_idx, SIMPLIFY = F)
