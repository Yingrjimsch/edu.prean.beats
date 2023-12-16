# verwendete Bibliotheken
library("shiny")
library("rpart")
library("rpart.plot")
library("MASS")

# Laden der Datensätze
#load("../data/spotify_songs_cleaned_with_trans.RData")
#load("../data/spotify_songs_cleaned_with_trans_optima.RData")
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
  # Auflistung aller Modell ohne Transformationen
  modelleListe <- list(
    "Regressionsbaum" = rt_model,
    "Bagged-Regressionsbaum" = bagged_rt_model,
    "Multiple lineare Regression" = lm_model,
    "k-Nearest Neighbors" = knn_model
  )
  
  # Auflistung aller Modell mit Transformationen
  modelleListeTrans <- list(
    "Regressionsbaum" = rt_model_trans,
    "Bagged-Regressionsbaum" = bagged_rt_model_trans,
    "Multiple lineare Regression" = lm_model_trans,
    "k-Nearest Neighbors" = knn_model_trans
  )
  
  # Auflistung aller verwendeter Resulsets ohne Transformationen
  resultsList <- list(
    "Multiple lineare Regression" = lm_model_results,
    "k-Nearest Neighbors" = knn_model_results,
    "Regressionsbaum" = rt_model_results,
    "Bagged-Regressionsbaum" = bagged_rt_model_results
  )
  
  # Auflistung aller verwendeter Resulsets mit Transformationen
  resultsListTrans <- list(
    "Multiple lineare Regression" = lm_model_trans_results,
    "k-Nearest Neighbors" = knn_model_trans_results,
    "Regressionsbaum" = rt_model_trans_results,
    "Bagged-Regressionsbaum" = bagged_rt_model_trans_results
  )
  
  # Auflistung aller Summary-Dateien ohne Transformationen
  summaryList <- list(
    "Multiple lineare Regression" = "www/lm_model_summary.txt",
    "k-Nearest Neighbors" = "www/knn_model_summary.txt",
    "Regressionsbaum" = "www/rt_model_summary.txt",
    "Bagged-Regressionsbaum" = "www/bagged_rt_model_summary.txt"
  )
  
  # Auflistung aller Summary-Dateien mit Transformationen
  summaryListTrans <- list(
    "Multiple lineare Regression" = "www/lm_model_trans_summary.txt",
    "k-Nearest Neighbors" = "www/knn_model_trans_summary.txt",
    "Regressionsbaum" = "www/rt_model_trans_summary.txt",
    "Bagged-Regressionsbaum" = "www/bagged_rt_model_trans_summary.txt"
  )
  
  #### TODO: kann erst verwendet werden nach merge und Erstellung aller Observed vs Predicted Plots
  
  # Auflistung aller Observed vs Predicted Plots-Dateien ohne Transformationen
  observedVsPredictedList <- list(
    "Multiple lineare Regression" = "lm_observed_vs_predicted.png",
    "k-Nearest Neighbors" = "knn_observed_vs_predicted.png",
    "Regressionsbaum" = "rt_observed_vs_predicted.png",
    "Bagged-Regressionsbaum" = "bagged_rt_observed_vs_predicted.png"
  )

  # Auflistung aller Observed vs Predicted Plots-Dateien mit Transformationen
  observedVsPredictedListTrans <- list(
    "Multiple lineare Regression" = "lm_trans_observed_vs_predicted.png",
    "k-Nearest Neighbors" = "knn_trans_observed_vs_predicted.png",
    "Regressionsbaum" = "rt_trans_observed_vs_predicted.png",
    "Bagged-Regressionsbaum" = "bagged_rt_trans_observed_vs_predicted.png"
  )
  
  # Plot des optimalen Regressionsbaums ohne Transformationen
  tree <- "optimalTree_without_trans.png"
  
  # Plot des optimalen Regressionsbaums mit Transformationen
  treeTrans <- "optimalTree_with_trans.png"
  
  # Auswahl des Datensatzes
  selectedDataOption <- reactive({
    input$datensatzAuswahl 
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
      if(input$gueteOptionen == "Summary") {
        # Auswahl der entsprechenden Summary-Liste
        summaryFileList <- if(selectedDataOption() == "Daten mit Transformationen") {
          summaryListTrans
        } else {
          summaryList
        }
        
        # Rendern von Text je nach ausgewählter Modellgüte
        statFile <- summaryFileList[[input$modellAuswahl]]
        cat("Ausgewählte Summary-Datei: ", statFile, "\n")
        if (!is.null(statFile) && file.exists(statFile)) {
          statText <- readLines(statFile)
          statHtml <- paste(statText, collapse = "<br>")
          HTML(statHtml)
        }
      } # Anzeige Predicted vs Observed
      else if(input$gueteOptionen == "Predicted vs Observed") {
        # Auswahl der entsprechenden Summary-Liste
        observedVsPredictedFileList <- if(selectedDataOption() == "Daten mit Transformationen") {
          observedVsPredictedListTrans
        } else {
          observedVsPredictedList
        }
        
        imgPath <- observedVsPredictedFileList[[input$modellAuswahl]]
        
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
         resultsList <- if(selectedDataOption() == "Daten mit Transformationen") {
           resultsListTrans
         } else {
           resultsList
         }
        results <- resultsList[[input$modellAuswahl]]
        #cat("Results: ", results$rmse)
        statHtml <- paste("<hr> MAE: ", results$mae, "<hr>", "MSE: ", results$mse, "<hr>" , "RMSE: ", results$rmse, collapse = "<hr>")
        HTML(statHtml)
      } # Anzeige des Tree Plots beim Regressionsbaumes
      else if("Regressionsbaum" %in% input$modellAuswahl && "Tree" %in% input$gueteOptionen) {
        pathRegressionsbaum <- if(selectedDataOption() == "Daten mit Transformationen") {
          treeTrans
        } else {
          tree
        }
        
        cat("Ausgewählte Datei: ", pathRegressionsbaum, "\n")
        
        if (!is.null(pathRegressionsbaum)) {
          completePath <- paste0(getwd(), '/www/', pathRegressionsbaum)
          if (file.exists(completePath)) {
            shiny::tags$img(src = pathRegressionsbaum, alt = "Regressionsbaum", width = "100%", height = "auto")
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
  
  ########## Panel Modellanwednung #########
  
  # Daten ohne Transformationen als Standard
  data <- reactive({
    return(spotify_songs_cleaned_without_trans)
  })
  
  # dynamischen UI-Prädiktoren- Elemente
  output$dynamischeInputs <- renderUI({
      data <- data()
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
  
  # Verarbeitung der Eingaben und zur Vorhersage
  reaktiveEingaben <- reactive({
      #cat("data: ", data, "\n")
      data <- data()
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
  
    # Vorhersage ohne transformierte Prädiktoren
    
    vorhersageErgebnis <- reactiveVal()
    
    observeEvent(input$vorhersageButton, {
      cat("Vorhersagebutton wurde gedrückt\n")
      
      ausgewaehltesModell <- modelleListe[[input$modellBestimmung]]
      cat("Ausgewähltes Modell: ", input$modellBestimmung, "\n")
      
      if(is.null(ausgewaehltesModell)){
        vorhersageErgebnis("Kein Modell vorhanden")
      }
      
      eingaben <- isolate(reaktiveEingaben())
      print(paste("Eingaben: ", eingaben))
      
      if (length(eingaben) == 0) {
        return("Keine Eingaben vorhanden")
      }
      
      tryCatch({
        vorhersage <- predict(ausgewaehltesModell, newdata =  eingaben)
        vorhersageString <- paste(vorhersage, collapse = " ")
        cat("Vorhersage: ", vorhersageString, "\n")
        vorhersageErgebnis(vorhersageString)
        
      }, error = function(e) {
        cat("Fehler bei der Vorhersage: ", e$message, "\n")
        vorhersageErgebnis(e$message)
      })
    })
  
    vorhersageErgebnisTrans <- reactiveVal()
    
    # Vorhersage mit transformierten Prädiktoren
    observeEvent(input$transformationButton, {
      cat("Vorhersagebutton mit Transformationen wurde gedrückt\n")
      
      ausgewaehltesModellTrans <- modelleListeTrans[[input$modellBestimmung]]
      cat("Ausgewähltes Modell mit Transformationen: ", input$modellBestimmung, "\n")
      
      if(is.null(ausgewaehltesModellTrans)){
        vorhersageErgebnis("Kein Modell mit Transformationen vorhanden")
      }
      
      eingaben <- isolate(reaktiveEingaben())
      print(paste("Eingaben vor Transformation: ", eingaben))
      
      if (length(eingaben) == 0) {
        return("Keine Eingaben vorhanden")
      }
      
      eingabenTransformiert <- transformiereEingaben(eingaben)
      
      tryCatch({
        vorhersage <- predict(ausgewaehltesModellTrans, newdata = eingabenTransformiert)
        vorhersageString <- paste(exp(vorhersage), collapse = " ")
        cat("Vorhersage mit Transformationen: ", vorhersageString, "\n")
        
        vorhersageErgebnisTrans(vorhersageString)
      }, error = function(e) {
        cat("Fehler bei der Vorhersage mit Transformationen: ", e$message, "\n")
        vorhersageErgebnisTrans(e$message)
      })
    })
  
    transformiereEingaben <- function(eingaben) {
      logPrädiktoren <- c("in_spotify_playlists", "bpm")
      boxcoxPrädiktor <- "energy_."
      
      # Log- Transformation und Prädiktorname anpassen
      for(prädiktor in logPrädiktoren) {
        if(any(eingaben[[prädiktor]] <= 0)) {
          stop("Eingabewerte für Logarithmus müssen positiv sein.")
        }
        eingaben[[paste0(prädiktor, "_log")]] <- log(eingaben[[prädiktor]])
        eingaben[[prädiktor]] <- NULL 
      }
      
      # Box-Cox-Transformation (Lambda 1.5) und Prädiktorname anpassen
      lambda <- 1.5
      eingaben[[paste0(boxcoxPrädiktor, "_boxcox")]] <- ifelse(lambda == 0, 
                                                               log(eingaben[[boxcoxPrädiktor]]),
                                                               (eingaben[[boxcoxPrädiktor]]^lambda - 1) / lambda)
      eingaben[[boxcoxPrädiktor]] <- NULL
      
      return(eingaben)
    }
  
    output$vorhersageOutputUI <- renderUI({
      if (!is.null(vorhersageErgebnis()) && input$vorhersageButton > input$transformationButton) {
        wellPanel(
          h3("Vorhersageergebnis ohne Transformation:"),
          p(vorhersageErgebnis(), style = "font-weight: bold;")
        )
      } else if (!is.null(vorhersageErgebnisTrans()) && input$transformationButton > input$vorhersageButton) {
        wellPanel(
          h3("Vorhersageergebnis mit Transformation:"),
          p(vorhersageErgebnisTrans(), style = "font-weight: bold;")
        )
      }
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