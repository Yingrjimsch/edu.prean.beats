# verwendete Bibliotheken
library("shiny")
library("rpart")
library("rpart.plot")

# Laden der Datensätze
#load("../data/spotify_songs_cleaned_with_trans.RData")
load("../data/spotify_songs_cleaned_with_trans_optima.RData")
load("../data/spotify_songs_cleaned_without_trans.RData")


# Laden der Modelle
lm_model <- readRDS("../data/RDataModels/regression/lm_model.rds")
lm_model_trans <- readRDS("../data/RDataModels/regression/lm_model_trans.rds")
knn_model <- readRDS("../data/RDataModels/knn/knn_model.rds")
knn_model_trans <- readRDS("../data/RDataModels/knn/knn_model_trans.rds")
rt_model <- readRDS("../data/RDataModels/regressionTree/rt_model.rds")
rt_model_trans <- readRDS("../data/RDataModels/regressionTree/rt_model_trans.rds")
bagged_rt_model <- readRDS("../data/RDataModels/baggedRegressionTree/bagged_rt_model.rds")
bagged_rt_model_trans <- readRDS("../data/RDataModels/baggedRegressionTree/bagged_rt_model_trans.rds")

# Laden der Results
lm_model_results <- readRDS("../data/RDataModels/regression/lm_model_results.rds")
lm_model_trans_results <- readRDS("../data/RDataModels/regression/lm_model_trans_results.rds")
knn_model_results <- readRDS("../data/RDataModels/knn/knn_model_results.rds")
knn_model_trans_results <- readRDS("../data/RDataModels/knn/knn_model_trans_results.rds")
rt_model_results <- readRDS("../data/RDataModels/regressionTree/rt_model_results.rds")
rt_model_trans_results <- readRDS("../data/RDataModels/regressionTree/rt_model_trans_results.rds")
bagged_rt_model_results <- readRDS("../data/RDataModels/baggedRegressionTree/bagged_rt_model_results.rds")
bagged_rt_model_trans_results <- readRDS("../data/RDataModels/baggedRegressionTree/bagged_rt_model_trans_results.rds")

# server
server <- function(input, output, session) {
  # Auflistung aller verwendeter Modelle, welche für die spätere Vorhersage benötigt werden
  modelleListe <- list(
    "Multiple lineare Regression" = lm_model_trans,
    "k-Nearest Neighbors" = knn_model_trans,
    "Regressionsbaum" = rt_model_trans,
    "Bagged-Regressionsbaum" = bagged_rt_model_trans
  )
  
  # Auflistung aller verwendeter Modelle, welche für die spätere Vorhersage benötigt werden
  resultsList <- list(
    "Multiple lineare Regression" = lm_model_results,
    "k-Nearest Neighbors" = knn_model_results,
    "Regressionsbaum" = rt_model_results,
    "Bagged-Regressionsbaum" = bagged_rt_model_results
  )
  
  #Auswahl des Datensatzes
  reactiveData <- reactive({
    aktiverTab <- ifelse(input$datensatzAuswahl2 != "", input$datensatzAuswahl2, input$datensatzAuswahl1)
    switch(aktiverTab,
           #"spotify_songs_cleaned_with_trans" = spotify_songs_cleaned_with_trans,
           "Daten mit Transformationen" = spotify_songs_cleaned_with_trans_optima,
           "Daten ohne Transformationen" = spotify_songs_cleaned_without_trans)
  })
  
  ########## Panel Home #########
  output$titelbild <- renderUI({
    imgPath <- "titelbild.png"
    shiny::tags$img(src = imgPath, alt = "Titelbild", width = "100%", height = "auto")
   })
  
  
  ########## Panel Modellleistung #########
  # Auswahl der Gueteoption
  output$modellGueteOptionen <- renderUI({
    if(input$modellAuswahl == "Multiple lineare Regression") {
      selectInput("gueteOptionen", "Wählen Sie die Modellgüte-Parameter:", 
                  choices = c("Predicted vs Observed", "Results", "Summary"))
    } else if(input$modellAuswahl == "k-Nearest Neighbors") {
      selectInput("gueteOptionen", "Wählen Sie die Modellgüte-Parameter:", 
                  choices = c("Predicted vs Observed", "Results", "Summary"))
    } else if(input$modellAuswahl == "Regressionsbaum") {
      selectInput("gueteOptionen", "Wählen Sie die Modellgüte-Parameter:", 
                  choices = c("Predicted vs Observed", "Results", "Summary", "Tree"))
    } else if(input$modellAuswahl == "Bagged-Regressionsbaum") {
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
  
  # Anzeige der verschiedenen Guetemasse
  output$dynamischeModellGuete <- renderUI({
    if (!is.null(input$gueteOptionen)) {
      # Anzeige Summary
      if(input$gueteOptionen == "Summary") {
        # Rendern von Text
        statFile <- switch(input$modellAuswahl,
                           "Multiple lineare Regression" = "www/lm_model_summary.txt",
                           "k-Nearest Neighbors" = "www/knn_model_summary.txt",
                           "Regressionsbaum" = "www/rt_model_summary.txt",
                           "Bagged-Regressionsbaum" = "www/bagged_rt_model_summary.txt",
                           NULL)
        cat("Ausgewählte Summary-Datei: ", statFile, "\n")
        if (!is.null(statFile) && file.exists(statFile)) {
          statText <- readLines(statFile)
          statHtml <- paste(statText, collapse = "<br>")
          HTML(statHtml)
        }
      } # Anzeige Predicted vs Observed
      else if(input$gueteOptionen == "Predicted vs Observed") {
        imgPath <- switch(input$modellAuswahl,
                          #"Multiple lineare Regression" = "www/lm__observed_vs_predicted.png",
                          #"k-Nearest Neighbors" = "www/knn__observed_vs_predicted.png",
                          "Regressionsbaum" = "rt__observed_vs_predicted.png",
                          "Bagged-Regressionsbaum" = "bagged_rt__observed_vs_predicted.png",
                          NULL)
        cat("Ausgewählte Datei: ", imgPath, "\n")
        
        if (!is.null(imgPath)) {
          # Überprüfen Sie die Datei mit dem vollständigen Pfad
          completePath <- paste0(getwd(), '/www/', imgPath)
          if (file.exists(completePath)) {
            shiny::tags$img(src = imgPath, alt = "Observed vs Predicted", width = "100%", height = "auto")
          } else {
            warning("Bild nicht gefunden: ", completePath)
            return(shiny::tags$p("Bild nicht gefunden."))
          }
        } 
      } # Anzeige der Results
       else if(input$gueteOptionen == "Results") {
        # Rendern von Text
        results <- resultsList[[input$modellAuswahl]]
        #cat("Results: ", results$rmse)
        statHtml <- paste("<hr> MAE: ", results$mae, "<hr>", "MSE: ", results$mse, "<hr>" , "RMSE: ", results$rmse, collapse = "<hr>")
        HTML(statHtml)
      } # Anzeige des Trees beim Regressionsbaumes
      else if("Regressionsbaum" %in% input$modellAuswahl && "Tree" %in% input$gueteOptionen) {
        imgPath <- "spotify_songs_cleaned_with_trans_optimal_tree.png"
        
        cat("Ausgewählte Datei: ", imgPath, "\n")
        
        if (!is.null(imgPath)) {
          completePath <- paste0(getwd(), '/www/', imgPath)
          if (file.exists(completePath)) {
            shiny::tags$img(src = imgPath, alt = "Regressionsbaum", width = "100%", height = "auto")
          } else {
            warning("Bild nicht gefunden: ", completePath)
            return(shiny::tags$p("Bild nicht gefunden."))
          }
        } 
      }
      else {
        HTML("<p>Bitte wählen Sie eine Option.</p>")
      }
    }
  })
  
  
########## Panel Modellanwendung #########
# Anzeige der Prädiktoren des ausgewählten Datensatzes
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
    cat("EingabeNamen: ", eingabenNamen, "\n")
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
      column(4, 
             actionButton("vorhersageButton", "Vorhersage ohne Transformation"),
             uiOutput("vorhersageOutputUI")
      ),
      column(4,
             actionButton("transformationButton", "Vorhersage mit Transformierten"))
    )
  })
  
  # Ausgabe des Vorhersageergebnisses
  output$vorhersageOutputUI <- renderUI({
    vorhersage <- vorhersageErgebnis()
    if (!is.null(vorhersage)) {
      wellPanel(
        h3("Vorhersageergebnis:"),
        p(vorhersage, style = "font-weight: bold;"),
        br()
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
    print(paste("Eingaben: ", eingaben))
    
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
  
  ########## Panel Über uns #########
  teamData <- reactive({
    data.frame(
      Name = c("Annaheim, Fabian C.", "Nobel Gabriel", "von Wartburg Rebekka", "Waldburger Safiyya"),
      EMail = c("annahfab@students.zhaw.ch", "nobelgab@students.zhaw.ch", "vonwareb@students.zhaw,ch", "waldbsaf@students.zhaw.ch")
    )
  })
  
  # Tabelle rendern
  output$team <- renderTable({
    teamData()
  })
  output$teambild <- renderUI({
    imgPath <- "teambild.png"
    shiny::tags$img(src = imgPath, alt = "Teambild", width = "50%", height = "auto")
  })
  
  
}