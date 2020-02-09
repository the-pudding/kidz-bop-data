potential_censored = c("drink","alcohol","beer","wine","champagne","ass","bitch","fuck","shit","damn","hell","sex","bottle","kiss","touch","hit","crazy","dope","liquor","overdose","intoxicate","gun","trigger","retard","pill","jesus","christ","grind","drug","bullet","dammit","god","gang","bang","nigga","booty","butt","breast","chest","drink","blow","kill","murder","skin","drunk","cock","pussy","devil","pimp","tit")

potential_censored = potential_censored  %>% sort()

potential_censored = data.frame(bad_word = potential_censored)
potential_censored$bad_word=as.character(potential_censored$bad_word)
write.csv(potential_censored, "data/2020-02-02/listOfBad.csv",row.names=F)


setwd("~/Desktop/kidz-bop-data")
censoredPhrases = read.csv("data/censoring/setdiffPhrasesPlus.csv",stringsAsFactors = F)


potential_censored$bad_word[which(potential_censored$bad_word == "hit")]="^hit"

censorIdx=lapply(potential_censored$bad_word, function(x){grep(x,censoredPhrases$censored_phrase)})

badWordPlus = cbind.data.frame(potential_censored, numSongs = unlist(lapply(censorIdx,length))) %>% arrange(desc(numSongs)) 

write.csv(badWordPlus, "data/2020-02-02/listOfBad.csv", row.names = F)                 
