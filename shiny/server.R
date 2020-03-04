library(diffobj)
server <- function(input, output, session) {
  
  load(file = "../data/2020-02-02/kbLyrics.RData")
  load(file = "../data/2020-02-02/ogLyricsFullPlain.RData")
  crosswalk = read.csv("../data/2020-02-02/crosswalk.csv", stringsAsFactors = F)
  #load(file = "../data/OG_artist_KB.RData")
  #load(file = "../data/OG_artist_name_KB.RData")
  #kb <- read.csv("../data/kbSongs.csv", stringsAsFactors = F)
  
  
  idx <- reactive({
    
    which(crosswalk$kb_song_name == input$songChoice)
    
  })
  
  output$diffobj_element <- renderUI({
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
  })
}