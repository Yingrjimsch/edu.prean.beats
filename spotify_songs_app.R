library("shiny")

load("./data/spotify_songs_cleaned_with_trans.RData")
load("./data/spotify_songs_cleaned_with_trans_optima.RData")
load("./data/spotify_songs_cleaned_without_trans.RData")


ui <- fluidPage(
  titlePanel("Regressionsmodell mit mehreren Prädiktoren"),
  sidebarLayout(
    sidebarPanel(
      # Input: Wählen Sie mehrere Variablen aus
      selectInput("variables", "Variablen wählen:", 
                  choices = colnames(iris)[-1], multiple = TRUE),
      # Button: Modell ausführen
      actionButton("goButton", "Modell ausführen")
    ),
    mainPanel(
      verbatimTextOutput("modelSummary")  # Output: Zusammenfassung des Modells
    )
  )
)

server <- function(input, output) {
  modelData <- eventReactive(input$goButton, {
    req(length(input$variables) > 0)  # Stellt sicher, dass Variablen ausgewählt wurden
    formulaStr <- paste("Sepal.Length ~", paste(input$variables, collapse = "+"))
    lm(as.formula(formulaStr), data = iris)
  })
  
  output$modelSummary <- renderPrint({
    req(modelData())
    summary(modelData())
  })
}

shinyApp(ui = ui, server = server)
