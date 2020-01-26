setwd("~/Desktop/KidzBopLyrics/data")
load(file = "ogLyrics.RData")
load(file="kbLyrics.RData")

library(diffobj)

test=diffPrint(target=as.data.frame(ogLyrics[[5]]), current=as.data.frame(kbLyrics[[5]]$result))

#https://stackoverflow.com/questions/2470248/write-lines-of-text-to-a-file-in-r

# by land
fileConn<-file("test.txt")
writeLines(ogLyrics[[8]]$line, fileConn)
close(fileConn)

fileConn<-file("test2.txt")
writeLines(kbLyrics[[8]]$result$line, fileConn)
close(fileConn)

# in bash 
# diff -u test.txt test2.txt > test3.txt

## by word
tryThis = lapply(ogLyrics[[8]]$line,strsplit," ")

fileConn<-file("testA.txt")
writeLines(unlist(tryThis), fileConn)
close(fileConn)

tryThis = lapply(kbLyrics[[8]]$result$line,strsplit," ")

fileConn<-file("testB.txt")
writeLines(unlist(tryThis), fileConn)
close(fileConn)

# in bash 
# diff -u testA.txt testB.txt > testC.txt

## this is getting warmer

## take everything with a minus in front of it as "censored"
#-drinking
#-burning
#-liquor