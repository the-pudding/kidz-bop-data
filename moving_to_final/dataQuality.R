# Oook! @Sara Stoudt I went through all of the songs between 0.0 and 0.4 in the new sheet and found about 39 that were mismatched. I also flagged quite a few that were missing Kidz Bop lyrics and some that were duplicates where one was correct but I couldn’t see the IDs or anything in the Shiny app so I just flagged them in the spreadsheet. Anyway, in the mismatched column, 0 means I checked and the original & KB songs matched and 1 means they didn’t match. Any left blank in that 0 - 0.4 range should have a note as to why I skipped it. If you have any questions lmk!

setwd("~/Desktop/kidz-bop-data/")
library(dplyr)
data = read.csv("moving_to_final/data/lineDifferencesFirstLine_update.csv", stringsAsFactors = F)

names(data)

data = data[,c(1,2,3,4,13,14)] 

unique(data$final_notes)

kbDrop = which(data$final_notes %in% c( "MISSING KB LYRICS", "KB LYRICS INCOMPLETE")) ## delete these

data[which(data$final_notes %in% c("MISSING OG LYRICS", "NO OG LYRICS")),]

data[221,] ## keep , everything else above drops

data[which(data$final_notes=="ERROR"),] %>% View()## should be able to fix these 

data[grep('DUPLICATE', data$final_notes),] # delete these 

data[grep("crush \\(david", data$final_notes),] ## delete


allData = read.csv("moving_to_final/data/allCensor.csv", stringsAsFactors = F)

allData2 = allData[-which(tolower(allData$song_name) %in% data[which(data$final_notes %in% c("MISSING KB LYRICS", "KB LYRICS INCOMPLETE" , "MISSING OG LYRICS", "NO OG LYRICS",'DUPLICATE', "(but \"crush (david archuletta)\" is right)","ERROR")  ),"song_name"]),]

allData3 = allData2[-which(tolower(allData2$song_name) %in% data[which(data$mismatch==1),"song_name"]),]

toFix = allData3[which(tolower(allData3$song_name) %in% c(data[which(data$final_notes == "ERROR"), "song_name"], "shine on (feat. mathias anderie)")),]

load(file = "data/2020-02-02/kbLyrics.RData")
load(file = "data/2020-02-02/ogLyricsFullPlain.RData")
load(file="data/missing/ogLyricsExtra.RData")
load(file="data/missing/kbLyricsExtraMatch.RData")

ogLyricsFull[[166]]
kbLyrics[[172]]

which(toFix$og_idx==166) ## wrong, no kb lyrics

#Nicki Minaj Fly

ogLyricsFull[[17]]
kbLyrics[[14]]

which(toFix$og_idx==14) ## wrong, no kb lyrics for this alive


ogLyricsFull[[179]]
kbLyrics[[184]]
## correct

ogLyricsFull[[16]]
kbLyrics[[14]] ## correct

ogLyricsFull[[180]]
kbLyrics[[184]] ## wrong, but missing lyrics

which(toFix$og_idx==180)

ogLyricsFull[[203]]
kbLyrics[[211]] ## wrong, no kidz bop

which(toFix$og_idx==203)

ogLyricsFull[[204]]
kbLyrics[[211]] ## wrong, not on genius

which(toFix$og_idx==204)

ogLyricsFull[[202]] ## correct

ogLyricsFull[[209]]
kbLyrics[[215]] ## correct

ogLyricsFull[[208]]
kbLyrics[[215]] ## wrong, missing lyrics

which(toFix$og_idx==208)


ogLyricsFull[[283]]
kbLyrics[[294]] ## wrong, no lyrics

which(toFix$og_idx==283)

ogLyricsFull[[284]]
kbLyrics[[294]] ## correct

ogLyricsFull[[286]] ## wrong
kbLyrics[[295]] ## wrong

which(toFix$og_idx==286)

ogLyricsFull[[285]]
kbLyrics[[295]] #correct


ogLyricsFull[[316]]
## missing

which(toFix$og_idx==316)

ogLyricsFull[[325]]
kbLyrics[[334]] ## wrong

which(toFix$og_idx==325)

ogLyricsFull[[324]]## no

which(toFix$og_idx==324)

## Mine Bazzi get original lyrics, kb 334

ogLyricsFull[[378]]
kbLyrics[[388]] ## wrong, no lyircs

which(toFix$og_idx==378)

ogLyricsFull[[379]] ## correct


## get shine on original lyrics kb 437

which(toFix$og_idx==669)

ogLyricsFull[[433]]
kbLyrics[[452]] ## wrong

which(toFix$og_idx==433)

ogLyricsFull[[434]] ## correct

ogLyricsFull[[478]]
kbLyrics[[503]] ## correct

ogLyricsFull[[477]] ## correct but missing kb

which(toFix$og_idx==477)

ogLyricsFull[[554]]
kbLyrics[[590]] ## correct

ogLyricsFull[[553]] ## no kidz bop

which(toFix$og_idx==553)

toRemove  = c(
  which(toFix$og_idx==166),
  which(toFix$og_idx==14),
  which(toFix$og_idx==180),
  which(toFix$og_idx==203),
  which(toFix$og_idx==204),
  which(toFix$og_idx==208),
  which(toFix$og_idx==283),
  which(toFix$og_idx==286),
  which(toFix$og_idx==316),
  which(toFix$og_idx==325),
  which(toFix$og_idx==324),
  which(toFix$og_idx==378),
  which(toFix$og_idx==669),
  which(toFix$og_idx==433),
  which(toFix$og_idx==477),
  which(toFix$og_idx==553)
  
)

toFix2 = toFix[-toRemove,]

fixed = rbind.data.frame(allData3, toFix2)

write.csv(fixed, "moving_to_final/data/allCensorF.csv",row.names=F)
