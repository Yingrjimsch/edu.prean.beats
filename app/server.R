library("shiny")


load("../data/spotify_songs_cleaned_with_trans.RData")
load("../data/spotify_songs_cleaned_with_trans_optima.RData")
load("../data/spotify_songs_cleaned_without_trans.RData")



# server
server <- function(input, output) {
  
  geladenes_modell <- readRDS("../data/RDatamodels/regressionTree/CART_regression_model_with_trans.rds")
  
  output$dynamischeInputs <- renderUI({
    spotify_songs_cleaned_with_trans <- spotify_songs_cleaned_with_trans[-which(names(spotify_songs_cleaned_with_trans) == "streams")]
    
    # Listen für numerische und faktorisierte Eingaben
    numerischeInputs <- list()
    faktorisierteInputs <- list()
    
    # Füllen der Listen basierend auf dem Variablentyp
    for(var in names(spotify_songs_cleaned_with_trans)) {
      if(is.numeric(spotify_songs_cleaned_with_trans[[var]])) {
        numerischeInputs[[var]] <- sliderInput(inputId = var, 
                                               label = paste(var),
                                               min = min(spotify_songs_cleaned_with_trans[[var]], na.rm = TRUE), 
                                               max = max(spotify_songs_cleaned_with_trans[[var]], na.rm = TRUE), 
                                               value = mean(spotify_songs_cleaned_with_trans[[var]], na.rm = TRUE))
      } else if(is.factor(spotify_songs_cleaned_with_trans[[var]])) {
        faktorisierteInputs[[var]] <- selectInput(inputId = var, 
                                                  label = paste(var),
                                                  choices = levels(spotify_songs_cleaned_with_trans[[var]]))
      }
    }
    
    faktorisierteInputs$vorhersageButton <- actionButton("vorhersageButton", "Vorhersage machen")
    
    # Darstellung der Prädiktorenauf zwei Spalten
    fluidRow(
      column(6, do.call(tagList, faktorisierteInputs)),
      column(6, do.call(tagList, numerischeInputs))
    )
  })
 # Wählen des Modells und der Gütemasse
  output$modellGueteOptionen <- renderUI({
    if(input$modellAuswahl == "Multivariate Regression") {
      checkboxGroupInput("gueteOptionen", "Wählen Sie die Modellgüte-Parameter:", 
                         choices = c("Parameter 1", "Parameter 2", "Parameter 3"))
    } else if(input$modellAuswahl == "k-Nearest Neighbors") {

      checkboxGroupInput("gueteOptionen", "Wählen Sie die Modellgüte-Parameter:", 
                         choices = c("Parameter 1", "Parameter 2", "Parameter 3"))
    } else if(input$modellAuswahl == "Regressionsbaum") {

      checkboxGroupInput("gueteOptionen", "Wählen Sie die Modellgüte-Parameter:", 
                         choices = c("Parameter 1", "Parameter 2", "Parameter 3"))
    }
    else if(input$modellAuswahl == "Bagged-Regressionsbaum") {
      checkboxGroupInput("gueteOptionen", "Wählen Sie die Modellgüte-Parameter:", 
                         choices = c("Parameter 1", "Parameter 2", "Parameter 3"))
    }
  })
  
  # Anzeige der Modellgüte basierend auf der Auswahl
  output$modellGueteErgebnis <- renderUI({
   HTML( paste0("<strong>Ausgewählter Datensatz:</strong><span style='margin-left: 25px;'>", input$datensatzAuswahl,
                "<br>",
          "<strong>Ausgewähltes Modell:</strong><span style='margin-left: 25px;'>", input$modellAuswahl, 
          "<br>",
          "<strong>Ausgewählte Modellgüte-Parameter:</strong><span style='margin-left: 25px;'>", paste(input$gueteOptionen, collapse = ", ")))
  })
  
  
  output$titelbild <- renderImage({
    list(src = "WWW/titelbild.png",
         width = "100%",
         height = "100%")
    
  }, deleteFile = FALSE)
  
}