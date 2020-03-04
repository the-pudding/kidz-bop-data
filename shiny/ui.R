kb <- read.csv("../data/kbSongs.csv", stringsAsFactors = F)

ui <- fluidPage(
  titlePanel("Compare Kidz Bop Lyrics to the Original"),
  fluidRow(
    column(
      3,
      wellPanel(
        hr("Pick a song"),
        selectInput("songChoice", "Original Song", c(crosswalk$kb_song_name,  song_name_extra),
                     multiple = FALSE,
                    selectize = TRUE, width = NULL, size = NULL
        )
      )
    ),
    column(
      9,
      htmlOutput("diffobj_element")
    )
  )
)