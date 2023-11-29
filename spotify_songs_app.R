library("shiny")

load("../data/spotify_songs_cleaned_with_trans.RData")
load("../data/spotify_songs_cleaned_with_trans_optima.RData")
load("../data/spotify_songs_cleaned_without_trans.RData")

# Benutzeroberfläche
ui <- fluidPage(
  titlePanel("Regressionsmodell mit mehreren Prädiktoren"),
  sidebarLayout(
    sidebarPanel(
      # Input
      selectInput("variables", "Variablen wählen:", 
                  choices = colnames(spotify_songs_cleaned_with_trans)[-1], multiple = TRUE),
      # Modell ausführen
      actionButton("goButton", "Modell ausführen")
    ),
    mainPanel(
      verbatimTextOutput("modelSummary")  # Output
    )
  )
)

# Server
server <- function(input, output) {
  modelData <- eventReactive(input$goButton, {
    req(length(input$variables) > 0) 
    formulaStr <- paste("streams ~ ", paste(input$variables, collapse = "+"))
    lm(as.formula(formulaStr), data = spotify_songs_cleaned_with_trans)
  })
  
  output$modelSummary <- renderPrint({
    req(modelData())
    summary(modelData())
  })
}

shinyApp(ui = ui, server = server)
