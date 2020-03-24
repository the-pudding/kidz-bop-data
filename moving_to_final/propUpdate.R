all = read.csv("moving_to_final/data/proportions-kb-prep3.csv", stringsAsFactors = F)


all$isCensored = ifelse(all$numOccurKB < all$numOccurOG, 1, 0)
all$isPresent = ifelse(all$numOccurOG>0, 1, 0)

require(ggplot2)
all %>% group_by(year) %>% summarise(prop=sum(isCensored)/n()) %>% ggplot(., aes(year, prop))+geom_point()

all$category = ifelse(all$category %in% c("profanity","slur"), "profanity", all$category)
all$category = ifelse(all$category %in% c("gender & sexuality"), "identity", all$category)
all$category = ifelse(all$category %in% c("gender & sexuality"), "identity", all$category)
all$category = ifelse(all$category %in% c("religious","mental health","other"), "other", all$category)

notC = read.csv("moving_to_final/data/notC.csv", stringsAsFactors = F)

helper <- function(idx){
  which(all$kb_idx == notC$kb_idx[idx] & all$og_idx==notC$og_idx[idx] & all$badword==notC$badword[idx])[1]
}
toRemove = lapply(1:nrow(notC), helper)

toRemove = unlist(toRemove)[which(!is.na(unlist(toRemove)))]

all2 = all[-toRemove,]

byYearGroup= all %>% group_by(year,category) %>% summarise(totalCensoredG= sum(isCensored), totalExistG = sum(isPresent)) 

propCensoredByYear = all %>% group_by(year) %>% summarise(propCensored = sum(isCensored)/sum(isPresent), totalCensored = sum(isCensored))

test = merge(byYearGroup, propCensoredByYear, by.x="year", by.y="year")
test$tryThis = test$totalCensoredG/test$totalCensored * test$propCensored

test2=subset(test, year ==2001)

sum(test2$tryThis)


require(tidyr)

#spread(test[,c("year","category","tryThis")], category, tryThis) %>% View()
apply(spread(test[,c("year","category","tryThis")], category, tryThis)[,-1] , 1, sum)

propCensoredByYear$propCensored

tidyV = spread(test[,c("year","category","tryThis")], category, tryThis)
tidyV = tidyV[,c("year","alcohol & drugs","sexual","profanity","violence","identity","other")]
names(tidyV)[2]="alcohol"

tidyV$alcohol = tidyV$alcohol *100
tidyV$sexual = tidyV$sexual *100
tidyV$profanity = tidyV$profanity *100
tidyV$identity = tidyV$identity *100
tidyV$other = tidyV$other *100

write.csv(tidyV, "moving_to_final/data/proportions-kb3.csv", row.names=F)


ggplot(test, aes(year, total))+geom_point()
