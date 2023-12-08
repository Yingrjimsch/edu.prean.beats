library("shiny")
library("rpart")
library("rpart.plot")


load("../data/spotify_songs_cleaned_with_trans.RData")
load("../data/spotify_songs_cleaned_with_trans_optima.RData")
load("../data/spotify_songs_cleaned_without_trans.RData")


#lm_model <- readRDS("../data/RDataModels/regressionTree/lm_model.rds")
#knn_model <- readRDS("../data/RDataModels/regressionTree/knn_model.rds")
rt_model <- readRDS("../data/RDataModels/regressionTree/rt_model.rds")
bagged_rt_model <- readRDS("../data/RDataModels/baggedRegressionTree/bagged_rt_model.rds")

#lm_model_results <- readRDS("../data/RDataModels/regressionTree/lm_model_results.rds")
#knn_model_results <- readRDS("../data/RDataModels/regressionTree/knn_model_results.rds")
rt_model_results <- readRDS("../data/RDataModels/regressionTree/rt_model_results.rds")
bagged_rt_model_results <- readRDS("../data/RDataModels/baggedRegressionTree/bagged_rt_model_results.rds")

# server
server <- function(input, output, session) {
  
  modelleListe <- list(
    #"Multivariate Regression" = lm_model,
    #"k-Nearest Neighbors" = knn_model,
    "Regressionsbaum" = rt_model,
    "Bagged-Regressionsbaum" = bagged_rt_model
  )
  
  resultsList <- list(
    #"Multivariate Regression" = lm_model_results,
    #"k-Nearest Neighbors" = knn_model_results,
    "Regressionsbaum" = rt_model_results,
    "Bagged-Regressionsbaum" = bagged_rt_model_results
  )
  
  reactiveData <- reactive({
    aktiverTab <- ifelse(input$datensatzAuswahl2 != "", input$datensatzAuswahl2, input$datensatzAuswahl1)
    switch(aktiverTab,
           "spotify_songs_cleaned_with_trans" = spotify_songs_cleaned_with_trans,
           "spotify_songs_cleaned_with_trans_optima" = spotify_songs_cleaned_with_trans_optima,
           "spotify_songs_cleaned_without_trans" = spotify_songs_cleaned_without_trans)
  })
  
  
  # Wählen des Modells und der Gütemasse
  output$modellGueteOptionen <- renderUI({
    if(input$modellAuswahl == "Multivariate Regression") {
      selectInput("gueteOptionen", "Wählen Sie die Modellgüte-Parameter:", 
                  choices = c("Predicted vs Observed", "Summary"))
    } else if(input$modellAuswahl == "k-Nearest Neighbors") {
      selectInput("gueteOptionen", "Wählen Sie die Modellgüte-Parameter:", 
                  choices = c("Predicted vs Observed", "Summary"))
    } else if(input$modellAuswahl == "Regressionsbaum") {
      selectInput("gueteOptionen", "Wählen Sie die Modellgüte-Parameter:", 
                  choices = c("Predicted vs Observed", "Results", "Summary", "Tree"))
    }
    else if(input$modellAuswahl == "Bagged-Regressionsbaum") {
      selectInput("gueteOptionen", "Wählen Sie die Modellgüte-Parameter:", 
                  choices = c("Predicted vs Observed", "Results", "Summary"))
    }
  })
  
  # Anzeige der Modellgüte basierend auf der Auswahl
  output$modellGueteErgebnis <- renderUI({
    HTML( paste0("<hr><strong>Ausgewählter Datensatz:</strong><span style='margin-left: 25px;'>", input$datensatzAuswahl1,
                 "<br>",
                 "<hr><strong>Ausgewähltes Modell:</strong><span style='margin-left: 25px;'>", input$modellAuswahl, 
                 "<br>",
                 "<hr><strong>Ausgewählte Modellgüte-Parameter:</strong><span style='margin-left: 25px;'>", paste(input$gueteOptionen, collapse = ", ")))
  })
  
  output$observedPredicted <- renderImage({
    if ("Predicted vs Observed" %in% input$gueteOptionen) {
      imgPath <- switch(input$modellAuswahl,
                        #"Regression" = "WWW/lm__observed_vs_predicted.png",
                        #"k-Nearest Neighbors" = "WWW/knn__observed_vs_predicted.png",
                        "Regressionsbaum" = "www/rt__observed_vs_predicted.png",
                        "Bagged-Regressionsbaum" = "www/bagged_rt__observed_vs_predicted.png",
                        NULL)
      cat("Ausgewählter Plot: ", imgPath, "\n")
      list(src = imgPath,width = "100%", height = "100%")
    }
  }, deleteFile = FALSE)
  
    
  output$results <- renderUI({
    if( "Results" %in% input$gueteOptionen){
      results <- resultsList[[input$modellAuswahl]]
      #cat("Results: ", results$rmse)
      statHtml <- paste("<hr> MAE: ", results$mae, "<hr>", "MSE: ", results$mse, "<hr>" , "RMSE: ", results$rmse, collapse = "<hr>")
        HTML(statHtml)
    }
  })
  
  output$summaryOutput <- renderUI({
    
    if (is.null(input$gueteOptionen) || "Summary" %in% input$gueteOptionen){
      statFile <- switch(input$modellAuswahl,
                         #"Multivariate Regression" = "../data/RDataModels/regression/lm_model_summary.txt",
                         #"k-Nearest Neighbors" = "../data/RDataModels/knn/knn_model_summary.txt",
                         "Regressionsbaum" = "../data/RDataModels/regressionTree/rt_model_summary.txt",
                         "Bagged-Regressionsbaum" = "../data/RDataModels/BaggedRegressionTree/bagged_rt_model_summary.txt",
                         NULL)
      cat("Ausgewählte summary-Datei: ", statFile, "\n")
      
      if (!is.null(statFile) && file.exists(statFile)) {
        statText <- readLines(statFile)
        statHtml <- paste(statText, collapse = "<br>")
        HTML(statHtml)
      }
    }
  })
  
  output$plotTree <- renderImage({
    if ("Tree" %in% input$gueteOptionen) {
      imgPath <- "www/spotify_songs_cleaned_with_trans_optimal_tree.png"
      if (!is.null(imgPath) && file.exists(imgPath)) {
        list(src = imgPath, contentType = 'image/png', alt = "Tree")
      } else {
        warning("Bild nicht gefunden: ", imgPath)
        return(NULL)
      }
    }
  }, deleteFile = FALSE)
  

  output$dynamischeInputs <- renderUI({
    data <- reactiveData()
    data <- data[-which(names(data) == "streams")] 
    
    # Listen für numerische und faktorisierte Eingaben
    faktorisierteInputs <- list()
    numerischeInputsLinks <- list()
    numerischeInputsRechts <- list()
    
    numerischeVars <- names(data)[sapply(data, is.numeric)]
    halbeLänge <- ceiling(length(numerischeVars) / 2)
    
    for(var in names(data)) {
      if(is.factor(data[[var]])) {
        faktorisierteInputs[[var]] <- selectInput(inputId = var, 
                                                  label = paste(var),
                                                  choices = levels(data[[var]]))
      } else if(is.numeric(data[[var]])) {
        if(which(var == numerischeVars) <= halbeLänge) {
          numerischeInputsLinks[[var]] <- sliderInput(inputId = var, 
                                                      label = paste(var),
                                                      min = min(data[[var]], na.rm = TRUE), 
                                                      max = max(data[[var]], na.rm = TRUE), 
                                                      value = mean(data[[var]], na.rm = TRUE))
        } else {
          numerischeInputsRechts[[var]] <- sliderInput(inputId = var, 
                                                       label = paste(var),
                                                       min = min(data[[var]], na.rm = TRUE), 
                                                       max = max(data[[var]], na.rm = TRUE), 
                                                       value = mean(data[[var]], na.rm = TRUE))
        }
      }
    }
    
    # Erstellen von drei Spalten
    fluidRow(
      column(4, do.call(tagList, numerischeInputsLinks)),
      column(4, do.call(tagList, numerischeInputsRechts)),
      column(4, do.call(tagList, faktorisierteInputs))
    )
  })
  
  reaktiveEingaben <- reactive({
    data <- reactiveData()
    #cat("data: ", data, "\n")
    eingabenWerte <- sapply(names(data)[-length(names(data))], function(x) input[[x]])
    eingabenNamen <- names(data)[-length(names(data))]
    #cat("EingabeNamen: ", eingabenNamen, "\n")
    eingabenDF <- setNames(as.data.frame(t(eingabenWerte), stringsAsFactors = FALSE), eingabenNamen)
    
    # Datentypen aus den Trainingsdaten ableiten und konvertieren
    for (spalte in names(eingabenDF)) {
      typ <- class(data[[spalte]])
      if (typ == "numeric") {
        eingabenDF[[spalte]] <- as.numeric(eingabenDF[[spalte]])
      } else if (typ == "integer"){
        eingabenDF[[spalte]] <- as.integer(eingabenDF[[spalte]])
      } else if (typ == "factor") {
        eingabenDF[[spalte]] <- factor(eingabenDF[[spalte]], levels = levels(data[[spalte]]))
      }
    }
    
    return(eingabenDF)
  })
  
  # Neuer Bereich für Vorhersagebutton und Output-Element
  output$vorhersageBereich <- renderUI({
    fluidRow(
      column(12, 
             actionButton("vorhersageButton", "Vorhersage machen"),
             hr(),
             uiOutput("vorhersageOutputUI")
      )
    )
  })
  
  # Ausgabe des Vorhersageergebnisses
  output$vorhersageOutputUI <- renderUI({
    vorhersage <- vorhersageErgebnis()
    if (!is.null(vorhersage)) {
      wellPanel(
        h5("Vorhersageergebnis:"),
        p(vorhersage, style = "font-weight: bold;"),
        hr()
      )
    }
  })
  
  vorhersageErgebnis <- eventReactive(input$vorhersageButton, {
    cat("Vorhersagebutton wurde gedrückt\n")
    
    ausgewaehltesModell <- modelleListe[[input$modellBestimmung]]
    # Modellname für Testzwecke ausgeben
    cat("Ausgewähltes Modell: ", input$modellBestimmung, "\n")
    
    # Überprüfen, ob ein Modell ausgewählt wurde
    if(is.null(ausgewaehltesModell)){
      return("Kein Modell vorhanden")
    }
    
    eingaben <- reaktiveEingaben()
    #print(paste("Eingaben: ", eingaben))
    
    # Überprüfen, ob alle Eingaben vorhanden sind
    if (length(eingaben) == 0) {
      return("Keine Eingaben vorhanden")
    }
    
    # Vorhersage durchführen
    tryCatch({
      vorhersage <- predict(ausgewaehltesModell, eingaben)
      vorhersageString <- paste(exp(vorhersage), collapse = " ")
      cat("Vorhersage: ", vorhersageString, "\n")
      
      return(as.character(vorhersageString))
    }, error = function(e) {
      cat("Fehler bei der Vorhersage: ", e$message, "\n")
      return(e$message)
    })
    
  })
  
  
  output$titelbild <- renderImage({
    list(src = "www/titelbild.png",
         width = "100%",
         height = "100%")
    
  }, deleteFile = FALSE)
  
}