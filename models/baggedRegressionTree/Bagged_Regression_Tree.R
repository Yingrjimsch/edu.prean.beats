##############################################################################


#                           Bagged CART Regressionsbaum


###############################################################################

###############################################################################

###################### Laden der notwendigen Libriaries #######################
# install.packages("rpart.plot") Install this package on first run
library("rpart")
library("rpart.plot")
library("dplyr")       
library("ipred")       
library("caret")       
library("Metrics")
library("ggplot2")
library("RColorBrewer")


######################### Einlesen der RData Dateien ##########################

load("../../data/spotify_songs_cleaned_with_trans.RData")
load("../../data/spotify_songs_cleaned_with_trans_optima.RData")
load("../../data/spotify_songs_cleaned_without_trans.RData")


# Kontrolle, ob Einlesen geklappt mittels anzeigen der Struktur der einzelnen Dataframes
str(spotify_songs_cleaned_with_trans)
str(spotify_songs_cleaned_with_trans_optima)
str(spotify_songs_cleaned_without_trans)


############################### Hilfsfunktionen ################################

# Standardisieren der numerischen Werte im Dataframe
scalingNumericalPredictors <- function(dataframe, target_var) {
  dataframe_scaled <- dataframe
  
  target_var_. <- dataframe[[target_var]]
  
  dataframe_scaled$target_var <- NULL
  
  dataframe_scaled <- as.data.frame(lapply(dataframe_scaled,
                                           function(x){
                                             if(is.numeric(x)) scale(x) else x
                                           }))
  dataframe_scaled[[target_var]] <- target_var_.
  
  return(dataframe_scaled = dataframe_scaled)
}


# Aufteilung der Daten in Trainings- und Testdatensatz
splittingDataframe <- function(dataframe, splitfactor) {
  
  set.seed(123) # Setzt einen Seed für reproduzierbare Ergebnisse
  index <- sample(1:nrow(dataframe), size = floor(splitfactor * nrow(dataframe)))
  trainData <- dataframe[index, ]
  testData <- dataframe[-index, ]
  return(list(trainData = trainData, testData = testData))
}

# Rücktransformierung der mittels Logarithmus transformierten Vorhersagen und aktuelle Werte
backtransformation <- function(pred, actual){
  
  log_pred <- pred 
  log_actual <- actual  
  
  pred <- exp(log_pred)
  actual <- exp(log_actual)
  return (list(pred = pred, actual = actual))
  
}

# bagging Methode
bagging <- function(dataframe, target_var,  ctrl, method, search) {
  
  formula <- as.formula(paste(target_var, "~."))
  bagged_tree <- train(
    formula,
    data = dataframe,
    search = search,
    method = method,
    trControl = ctrl,
    importance = TRUE
  )
}

# Erstellen von Plots der Gütemasse (auskommentiert, da Werte zu hoch und dadurch deren Unterschiede in einem Diagramm kaum ersichtlich sind)
# plottingQualityMass <- function(qualityVector, value, savePath) {
#   color <- brewer.pal(6, "PRGn")
#   # Umwandeln der Liste in einen Dataframe
#   qualityDf <- data.frame(Model = names(qualityVector), value = qualityVector)
#   
#   # Erstellen des Plots
#   plot <- ggplot(qualityDf, aes(x = Model, y = value, fill = Model)) +
#     geom_bar(stat = "identity") +
#     scale_fill_manual(values = color) +
#     theme_minimal() +
#     theme(axis.text.x = element_blank()) +
#     labs(title = paste("Vergleich der", value, "-Werte verschiedener Modelle"),
#          x = "Modell",
#          y = value)
#   
#   # Speichern des Plots als PNG
#   ggsave(filename = paste0("Bagged_RegressionTree/quality_comparison_", value, ".png"),
#          plot = plot,
#          device = "png",
#          width = 10,
#          height = 6,
#          units = "in")
# }



################################ Main Funktion ################################


generateBaggedRegressionTree <- function(dataframe, splitfactor, target_var, method, ctrl, search, scaling = TRUE, isTargetTransformed = TRUE) {
  set.seed(123)
  folder_name <- "../../models/baggedRegressionTree/img"
  
  if (!dir.exists(folder_name)) {
    
    dir.create(folder_name)
    
  }
  
  file_name <- deparse(substitute(dataframe))
  
  path_name <-paste0(folder_name, "/", file_name )
  
  if(scaling){
      dataframe <- scalingNumericalPredictors(dataframe = dataframe, target_var = target_var) 
    }
    splittedData <- splittingDataframe(dataframe = dataframe, splitfactor = splitfactor )
    trainData <- splittedData$trainData
    testData <- splittedData$testData
    
    bagged_tree <- bagging(trainData, target_var,  ctrl, method, search)
    print(bagged_tree)
    
    # Plot der entscheidensten Prädiktoren
    full_filename <- paste0(path_name, "_important_vars.png")
    png(file = full_filename, width = 800, height = 600)
    plot(varImp(bagged_tree), 20, main = paste0(file_name, "_important_vars.png"))
    
    pred <- predict(bagged_tree, newdata = testData)
    actual <- testData[[target_var]]

    
    # falls Dataset mit log transformierter Zielvariable verwendet wird
    if(isTargetTransformed){
      backtrans <- backtransformation(pred, actual)
      pred <- backtrans$pred
      actual <- backtrans$actual
    }
    
    # Berechnung des MAE, MSE, RMSE
    mae <- mean(abs(pred - actual))
    mse <- mean((pred - actual)^2)
    rmse <- sqrt(mse)
    print(paste0("mae: ", mae))
    print(paste0("mse ", mse))
    print(paste0("rmse ", rmse))
    
    return(list(bagged_tree = bagged_tree, mae = mae, mse = mse, rmse = rmse))
  }
  


###################################################################################################################

# Anwenden an alle Dataframes


###################################################################################################################

# MSEs <- c() # Vektor um alle MSE's der Modelle abzulegen
# RMSEs <- c() # Vektor um alle RMSE's der Modelle abzulegen

# Steuerung des Trainingsprozesses mit crossvalidation (cv) und einer Anzahl von k-folds (k = 15)
ctrl <- trainControl(method = "cv",  number = 15) 

####################################################################################################################

#           Bestes Ergebnis mit Dataset "spotify_songs_cleaned_with_trans" (mit scaling)


####################################################################################################################

result_with_trans_with_scaling <- generateBaggedRegressionTree(spotify_songs_cleaned_with_trans, 0.8, "streams", "treebag", ctrl, "grid", scaling = TRUE, isTargetTransformed = TRUE )

print(paste("MAE (with transformation with & with scaling): ", result_with_trans_with_scaling$mae))
print(paste("MSE (with transformation with & with scaling): ", result_with_trans_with_scaling$mse))
print(paste("RMSE (with transformation with & with scaling): ", result_with_trans_with_scaling$rmse))

# MSEs["spotify_songs_cleaned_with_trans_with_scaling"] <- result_with_trans_with_scaling$mse
# RMSEs["spotify_songs_cleaned_with_trans_with_scaling"] <- result_with_trans_with_scaling$rmse

####################################################################################################################

#           Bestes Ergebnis mit Dataset "spotify_songs_cleaned_with_trans" (ohne scaling)


####################################################################################################################

result_with_trans_without_scaling <- generateBaggedRegressionTree(spotify_songs_cleaned_with_trans, 0.8, "streams", "treebag", ctrl, "grid", scaling = FALSE, isTargetTransformed = TRUE )

print(paste("MAE (with transformation without & without scaling): ", result_with_trans_without_scaling$mae))
print(paste("MSE (with transformation without & without scaling): ", result_with_trans_without_scaling$mse))
print(paste("RMSE (with transformation without & without scaling): ", result_with_trans_without_scaling$rmse))

# MSEs["spotify_songs_cleaned_with_trans_without_scaling"] <- result_with_trans_without_scaling$mse
# RMSEs["spotify_songs_cleaned_with_trans_without_scaling"] <- result_with_trans_without_scaling$rmse

####################################################################################################################

#           Bestes Ergebnis mit Dataset "spotify_songs_cleaned_with_trans_optima" (mit scaling)


####################################################################################################################

result_with_trans_optima_with_scaling <- generateBaggedRegressionTree(spotify_songs_cleaned_with_trans_optima, 0.8, "streams", "treebag", ctrl, "grid", scaling = TRUE, isTargetTransformed = TRUE )

bagged_rt_model_trans <- result_with_trans_optima_with_scaling$bagged_tree
writeLines(capture.output(bagged_rt_model_trans), "../../data/RDataModels/baggedRegressionTree/bagged_rt_model_trans_summary.txt")
saveRDS(bagged_rt_model_trans, "../../data/RDataModels/baggedRegressionTree/bagged_rt_model_trans.rds")
saveRDS(result_with_trans_optima_with_scaling, "../../data/RDataModels/baggedRegressionTree/bagged_rt_model_trans_results.rds")

print(paste("MAE (with transformation with & with scaling): ", result_with_trans_optima_with_scaling$mae))
print(paste("MSE (with transformation with & with scaling): ", result_with_trans_optima_with_scaling$mse))
print(paste("RMSE (with transformation with & with scaling): ", result_with_trans_optima_with_scaling$rmse))

# MSEs["spotify_songs_cleaned_with_trans_optima_with_scaling"] <- result_with_trans_optima_with_scaling$mse
# RMSEs["spotify_songs_cleaned_with_trans_optima_with_scaling"] <- result_with_trans_optima_with_scaling$rmse


####################################################################################################################

#           Bestes Ergebnis mit Dataset "spotify_songs_cleaned_with_trans_optima" (ohne scaling)


####################################################################################################################

result_with_trans_optima_without_scaling <- generateBaggedRegressionTree(spotify_songs_cleaned_with_trans_optima, 0.8, "streams", "treebag", ctrl, "grid", scaling = FALSE, isTargetTransformed = TRUE )

print(paste("MAE (with transformation without & without scaling): ", result_with_trans_optima_without_scaling$mae))
print(paste("MSE (with transformation without & without scaling): ", result_with_trans_optima_without_scaling$mse))
print(paste("RMSE (with transformation without & without scaling): ", result_with_trans_optima_without_scaling$rmse))

# MSEs["spotify_songs_cleaned_with_trans_optima_without_scaling"] <- result_with_trans_optima_without_scaling$mse
# RMSEs["spotify_songs_cleaned_with_trans_optima_without_scaling"] <- result_with_trans_optima_without_scaling$rmse

####################################################################################################################

#           Bestes Ergebnis mit Dataset "spotify_songs_cleaned_without_trans" (mit scaling)


####################################################################################################################

result_without_trans_with_scaling <- generateBaggedRegressionTree(spotify_songs_cleaned_without_trans, 0.8, "streams", "treebag", ctrl, "grid", scaling = TRUE, isTargetTransformed = FALSE )


bagged_rt_model <- result_without_trans_with_scaling$bagged_tree
writeLines(capture.output(bagged_rt_model), "../../data/RDataModels/baggedRegressionTree/bagged_rt_model_summary.txt")
saveRDS(bagged_rt_model, "../../data/RDataModels/baggedRegressionTree/bagged_rt_model.rds")
saveRDS(result_without_trans_with_scaling, "../../data/RDataModels/baggedRegressionTree/bagged_rt_model_results.rds")


print(paste("MAE (without transformation with & with scaling): ", result_without_trans_with_scaling$mae))
print(paste("MSE (without transformation with & with scaling): ", result_without_trans_with_scaling$mse))
print(paste("RMSE (without transformation with & with scaling): ", result_without_trans_with_scaling$rmse))

# MSEs["spotify_songs_cleaned_without_trans_with_scaling"] <- result_without_trans_with_scaling$mse
# RMSEs["spotify_songs_cleaned_without_trans_with_scaling"] <- result_without_trans_with_scaling$rmse

####################################################################################################################

#           Bestes Ergebnis mit Dataset "spotify_songs_cleaned_without_trans" (ohne scaling)


####################################################################################################################

result_without_trans_without_scaling <- generateBaggedRegressionTree(spotify_songs_cleaned_without_trans, 0.8, "streams", "treebag", ctrl, "grid", scaling = FALSE, isTargetTransformed = FALSE )

print(paste("MAE (without transformation with & without scaling): ", result_without_trans_without_scaling$mae))
print(paste("MSE (without transformation with & without scaling): ", result_without_trans_without_scaling$mse))
print(paste("RMSE (without transformation with & without scaling): ", result_without_trans_without_scaling$rmse))

# MSEs["spotify_songs_cleaned_without_trans_without_scaling"] <- result_without_trans_without_scaling$mse
# RMSEs["spotify_songs_cleaned_without_trans_without_scaling"] <- result_without_trans_without_scaling$rmse

# plottingQualityMass(MSEs, "MSE")
# plottingQualityMass(RMSEs, "RMSE")



