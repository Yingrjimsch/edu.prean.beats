library("shiny")
library("shinythemes")
library("bslib")

load("../data/spotify_songs_cleaned_with_trans.RData")
load("../data/spotify_songs_cleaned_with_trans_optima.RData")
load("../data/spotify_songs_cleaned_without_trans.RData")

# ui
ui <- navbarPage(
  hr(),
  theme = bs_theme(bootswatch = "vapor"),
  tags$head(
    tags$style(HTML("
            .navbar {
                height: 70px;
            }
            .navbar-brand, .navbar-nav li a {
                line-height: 70px;
            }
        "))
  ),
  title = strong("Von Beats zur Beliebtheit"),
           tabPanel(title = "Home", 
                    p(style="text-align: center;",
                      strong("Semesterarbeit Modul Predictive Analytics HS23")),
                    imageOutput("titelbild"),
                    hr(),
                    p(style="text-align: center;","Annaheim, Fabian C. | Nobel, Gabriel | von Wartburg Rebekka | Waldburger, Safiyya")
                      ),
           tabPanel(title = "Bedienungsanleitung",
                    h4(strong("Beschreibung...")),
                    p(style="text-align: justify; font-size = 25px",
                      "Lorem ipsum dolor sit amet, consetetur sadipscing elitr,
                        sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat,
                        sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum.
                        Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.
                        Lorem ipsum dolor sit amet,
                        consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat,
                        sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum.
                        Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet."),
                    hr()),
           tabPanel(title = "Modellanwendung",
                    uiOutput("dynamischeInputs"),
                        mainPanel(
                          
                        )
                      )
                    ,
           tabPanel(title = "Über uns",
                    "Annaheim, Fabian C. | Nobel, Gabriel | von Wartburg Rebekka | Waldburger, Safiyya")
     )


# server
server <- function(input, output) {
  output$dynamischeInputs <- renderUI({
    spotify_songs_cleaned_without_trans <- spotify_songs_cleaned_without_trans[-which(names(spotify_songs_cleaned_without_trans) == "streams")]
    inputList <- lapply(names(spotify_songs_cleaned_without_trans), function(var) {
      if(is.numeric(spotify_songs_cleaned_without_trans[[var]])) {
        sliderInput(inputId = var, 
                    label = paste(var),
                    min = min(spotify_songs_cleaned_without_trans[[var]], na.rm = TRUE), 
                    max = max(spotify_songs_cleaned_without_trans[[var]], na.rm = TRUE), 
                    value = mean(spotify_songs_cleaned_without_trans[[var]], na.rm = TRUE))
      } else if(is.factor(spotify_songs_cleaned_without_trans[[var]])) {
        selectInput(inputId = var, 
                    label = paste(var),
                    choices = levels(spotify_songs_cleaned_without_trans[[var]]))
      }
    })
    do.call(tagList, inputList)
  })
  
  
  output$titelbild <- renderImage({
    list(src = "WWW/titelbild.png",
         width = "100%",
         height = "100%")
    
  }, deleteFile = FALSE)
  
}


shinyApp(ui = ui, server = server)


