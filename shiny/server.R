library(diffobj)
server <- function(input, output, session) {
  
  load(file = "../data/2020-02-02/kbLyrics.RData")
  load(file = "../data/2020-02-02/ogLyricsFullPlain.RData")
  load(file="../data/missing/ogLyricsExtra.RData")
  load(file="../data/missing/kbLyricsExtraMatch.RData")
  crosswalk = read.csv("../data/2020-02-02/crosswalk.csv", stringsAsFactors = F)
  #load(file = "../data/OG_artist_KB.RData")
  #load(file = "../data/OG_artist_name_KB.RData")
  #kb <- read.csv("../data/kbSongs.csv", stringsAsFactors = F)
  song_name_extra = unlist(lapply(lapply(kbLyricsExtra, function(x){x$result$song_name[1]}), function(x){ifelse(is.null(x),NA, x)}) )
  
  idx <- reactive({
    
    c1= which(crosswalk$kb_song_name == input$songChoice)
    c2= which(song_name_extra == input$songChoice)
    if(length(c1)==0){
      return(c2)
    }else{
      return(c1)
    }
  })
  
  whichData <- reactive({
    c1= which(crosswalk$kb_song_name == input$songChoice)
    c2= which(song_name_extra == input$songChoice)
    if(length(c1)==0){
      return(F)
    }else{
      return(T)
    }
  })
  
  output$diffobj_element <- renderUI({
    if(whichData()){
    HTML(
      as.character(
        diffPrint(
          as.data.frame(ogLyricsFull[[crosswalk$og_idx[idx()]]]$line), as.data.frame(kbLyrics[[crosswalk$kb_idx[idx()]]]$line),
          mode = "sidebyside",
          format = "html", 
          style = list(html.output = "page") #diff.w.style
        )
      )
    )
    }else{
      HTML(
        as.character(
          diffPrint(
            as.data.frame(ogLyricsExtra[[idx()]]$result$line), as.data.frame(kbLyricsExtra[[idx()]]$result$line),
            mode = "sidebyside",
            format = "html", 
            style = list(html.output = "page") #diff.w.style
          )
        )
      )
    }
  })
}