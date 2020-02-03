setwd("/Users/Sara/Desktop/kidz-bop-data/")

#### all available of interest on genius ####

pulled <- read.csv("data/pulledSongs.csv", stringsAsFactors = F)
toUse = pulled[-which(pulled$remove == "X"),] ## get rid of ones we don't want
grabbable = toUse[!is.na(toUse$song_id),] ## which ones are on genius?

# grab lyrics by song id?

library(geniusr)
genius_token()
kbLyrics <- lapply(grabbable$song_id, function(x) {
  scrape_lyrics_id(song_id = x)
})
save(kbLyrics, file = "data/2020-02-02/kbLyrics.RData")


write.csv(cbind.data.frame(idx = 1:nrow(grabbable), kb_song_name = grabbable$song_name), "data/2020-02-02/kbSongNames.csv",row.names = F)

og <- read.csv("data/Discography_Manual_Check_artistCheck.csv", stringsAsFactors = F)
load(file="OG_info_fromKBGenius.RData")
lyrics = unlist(lapply(getOG, function(x){x$song_name[1]}))

lyrics = tolower(lyrics)
og$kb_song_name=tolower(og$kb_song_name)

write.csv(cbind.data.frame(idx = 1:nrow(og), og_song_name = og$song_name), "data/2020-02-02/ogSongNames.csv",row.names = F)


og_song_id = rep(NA, nrow(og))
for(i in 1:nrow(og)){
  if(og$isCorrect[i]=="Y"){
    if(length(which(lyrics == og$kb_song_name[i]))==0){
      
    }else if(length(which(lyrics == og$kb_song_name[i]))>1){
      og_song_id[i]= -9999
    }else{
      og_song_id[i]=getOG[[which(lyrics == og$kb_song_name[i])]]$song_id[1]
      
    }
  }else{
    og_song_id[i] = og$song_id[i]
  }
  print(i)
}

which(og_song_id==-9999)
og$kb_song_name[which(is.na(og_song_id))]

# 49 remix ft kendrick lamar
fillIn = c(NA, NA, 15, 27, 31, 43, 49, 59, 60, 62, 
  75, 92, 96, 227, 99, 109, 115, 118, NA, NA,
  122, 126, 128, 127, 129, NA, 135, 140, 142, NA,
  NA, NA, 153, 158, NA, 162, NA, 176, 184, 187,
  192, 203, 200,  NA, 222, 229, 233, 252, 253, 265,
  272, NA, NA, 303, NA, 316, 317, NA, NA, 345,
  346, 356, 357, 363, 365, 366, 370, 376, NA, NA,
  396, NA, NA, 400, NA, 415, 418, 423, NA, NA, 
  429, 432, 438, NA, 445, 447, NA, 466, 467, 469, 
  NA , 479, 483, 485, NA, 490, 492, 507 , 512, 514,
  422, NA, NA, NA, NA, 550, 554, 556, 564, 568,
  NA, 596, 606, NA, 610, 613, 616, 620, NA, 632, 
  NA, 634, 637, 638, 641, NA, NA, NA, 659, NA, 
  681, 690, 695, 706) #%>% length()

helper <- function(x){
  if(is.na(x)){
    return(NA)
  }else{
    return(getOG[[x]]$song_id[1])
  }
}

og_song_id[which(is.na(og_song_id))]=unlist(lapply(fillIn, helper))

which(og_song_id=="-9999")

og[which(og_song_id=="-9999"),]
og_song_id[which(og_song_id=="-9999")] = c(2970719,2998843)

summary(og_song_id)

og$kb_song_name[which(is.na(og_song_id))]

missing = c("13852","3114109","515319","2391084","2481759",
            "52466", "3088967","21991","513887", "121177",
            "690706", "2894765", "84166", "709053", "503608",
            "2855384", "697785","665379", "193598", "2329765",
            "198751", "2407925","199794", "198944","197591",
            "134525","725792","4641031","474036","2374092",
           "52706", "77125","347229", "579177","2399497",
           "66873","720259","66302","209173")
length(missing)
which(is.na(og_song_id)) %>% length()

og_song_id[which(is.na(og_song_id))]=missing


library(purrr)

safe_scrape <- safely(scrape_lyrics_id) ## make sure if we can't find a song's lyrics, it doesn't crash

ogLyrics <- lapply(og_song_id, function(x) {
  safe_scrape(song_id = x)
})

save(ogLyrics, file = "data/2020-02-02/ogLyrics.RData")

test = lapply(ogLyrics, function(x){x$result})

ogLyrics = test

unlist(lapply(test, nrow)) %>% summary()

save(ogLyrics, file = "data/2020-02-02/ogLyricsPlain.RData")

setdiff(og$kb_song_name, tolower(grabbable$song_name)) %>% length() ## Have 100 extra
setdiff(tolower(grabbable$song_name), og$kb_song_name) %>% length() ## missing 152
intersect(tolower(grabbable$song_name), og$kb_song_name) %>% length() ## 451 overlap

# need song ids for these
setdiff(tolower(grabbable$song_name), og$kb_song_name)

missing = c("67773","83049", "153803", "32015", "20418",
  "857127", "3684536", "3069729","3221361", "198446", 
  "154924", "542389", "57225", "63759", "204330",
  "582650", "67202", "183325", "174671","2949338", 
  "2977709", "488123", "145495", "32167", "226895",
  "198660", "1236", "2416822", "2851057", "3828011",
  "3281773", "120574", "72043", "2898791", "430725",
  "1003672", "3348299", "835434", "652107", "3315890",
  "3207340", "371292", "117023", "503772", "3207820",
  "491416", "3265643", "4690095", "372835", "65651",
  "735642", "396337", "336227", "3004837", "773391", 
  "623112", "2549",  "707165", "724006", "4140",
  "217401", "1698522", NA, "548094", "187904",
  "484596", "434224", "420298", "3592379",
  "207394", "501510", "2378935", "206622", "95389",  "2416274",
  "3844672", "224519", "1411", "2274681", "1527359",
  "3988", "68946", "2509", "42105", "2953761",
  "174292", "152470", "116960", "3221550", "104606",
  "2117963", "260497", "156070", "2119451", "1249145",
  "2853060", "18601", "62493", "328807", "407609",
  NA, "50873", "3103488", "85753", "114915",
  "290234", "65848", "849292", "174702", NA,
  "522811", "135048", "157814", "245655", "58062",
  "57189", "3203937", "4063065", "2458377", "820404", 
  "438004", "214369", "570363", "493162", "437103", 
  "336288", "3882", "58888", "3219597", "100386",
  "912255", "2285826", "305745", "157246", "1743010",
  "2388373", "275140", "1618449", "72605", NA,  
  "1319398", "2286779", "330963", "1118662", "174690",
  "5944", "51128", "378409", "1395376", "273135", "1889", "52857"
  
  
  )
length(missing)
setdiff(tolower(grabbable$song_name), og$kb_song_name) %>% length()
## HERE
og2 = c(og$kb_song_name, setdiff(tolower(grabbable$song_name), og$kb_song_name))
write.csv(cbind.data.frame(idx = 1:length(og2), og_song_name = og2), "data/2020-02-02/ogSongNamesMore.csv",row.names = F)


ogLyrics <- lapply(missing, function(x) {
  safe_scrape(song_id = x)
})

save(ogLyrics, file = "data/2020-02-02/ogLyrics2.RData")

test = lapply(ogLyrics, function(x){x$result})

ogLyrics = test

unlist(lapply(test, nrow)) %>% summary()

save(ogLyrics, file = "data/2020-02-02/ogLyrics2Plain.RData")


load(file="data/2020-02-02/ogLyricsPlain.RData")

ogLyrics1 = ogLyrics

load(file="data/2020-02-02/ogLyrics2Plain.RData")

ogLyrics2 = ogLyrics

test = c(ogLyrics1, ogLyrics2)

ogLyricsFull = test
save(ogLyricsFull, file="data/2020-02-02/ogLyricsFullPlain.RData")
