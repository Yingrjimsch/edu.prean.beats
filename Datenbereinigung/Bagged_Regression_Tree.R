##############################################################################


#                           Bagged CART Regressionsbaum


###############################################################################

###############################################################################

###################### Laden der notwendigen Libriaries #######################

library("rpart")
library("rpart.plot")
library("dplyr")       
library("ipred")       
library("caret")       
library("Metrics")
library("ggplot2")


######################### Einlesen der RData Dateien ##########################

load("spotify_songs_cleaned_with_trans.RData")
load("spotify_songs_cleaned_with_trans_optima.RData")
load("spotify_songs_cleaned_without_trans.RData")


# Kontrolle, ob Einlesen geklappt mittels anzeigen der Struktur der einzelnen Dataframes
str(spotify_songs_cleaned_with_trans)
str(spotify_songs_cleaned_with_trans_optima)
str(spotify_songs_cleaned_without_trans)


############################### Hilfsfunktionen ################################



# Aufteilung der Daten in Trainings- und Testdatensatz

splittingDataframe <- function(dataframe, splitfactor) {
  
  set.seed(123) # Setzt einen Seed für reproduzierbare Ergebnisse
  index <- sample(1:nrow(dataframe), size = floor(splitfactor * nrow(dataframe)))
  trainData <- dataframe[index, ]
  testData <- dataframe[-index, ]
  return(list(trainData = trainData, testData = testData))
}

backtransformation <- function(pred, actual){
  
  log_pred <- pred 
  log_actual <- actual  
  
  pred <- exp(log_pred)
  actual <- exp(log_actual)
  return (list(pred = pred, actual = actual))
  
}

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

plottingQualityMass <- function(rmseVector, value) {
  # Umwandeln der Liste in einen Dataframe
  rmseDf <- data.frame(Model = names(RMSEs), RMSE = unlist(RMSEs))
  
  ggplot(rmseDf, aes(x = Model, y = RMSE, fill = Model)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = "Vergleich der RMSE-Werte verschiedener Modelle",
         x = "Modell",
         y = value)
}




################################ Main Funktion ################################


generateBaggedRegressionTree <- function(dataframe, splitfactor, target_var, method, ctrl, search, scaling = TRUE, isTargetTransformed = TRUE) {
    if(scaling){
      dataframe <- scalingNumericalPredictors(dataframe = dataframe, target_var = target_var) 
    }
    splittedData <- splittingDataframe(dataframe = dataframe, splitfactor = splitfactor )
    trainData <- splittedData$trainData
    testData <- splittedData$testData
    
    bagged_tree <- bagging(trainData, target_var,  ctrl, method, search)
    
    # Plot der entscheidensten Prädiktoren
    plot(varImp(bagged_tree), 20)
    
    pred <- predict(bagged_tree, newdata = testData)
    actual <- testData[[target_var]]
    
    if(isTargetTransformed){
      backtrans <- backtransformation(pred, actual)
      pred <- backtrans$pred
      actual <- backtrans$actual
    }
    
    # Berechnung des RMSE
    mse <- mean((pred - actual)^2)
    rmse <- sqrt(mse)
    
    return(list(bagged_tree = bagged_tree, mse = mse, rmse = rmse))
  }
  



###################################################################################################################

# Anwenden an alle Dataframes


###################################################################################################################

RMSEs <- c() # Vektor um alle RMSE's der Modelle abzulegen
ctrl <- trainControl(method = "cv",  number = 40) 

####################################################################################################################

#           Bestes Ergebnis mit Dataset "spotify_songs_cleaned_with_trans" (mit scaling)


####################################################################################################################

result_with_trans_with_scaling <- generateBaggedRegressionTree(spotify_songs_cleaned_with_trans, 0.8, "streams", "treebag", ctrl, "grid", scaling = TRUE, isTargetTransformed = TRUE )

print(paste("MSE (with transformation with & with scaling): ", result_with_trans_with_scaling$mse))
print(paste("RMSE (with transformation with & with scaling): ", result_with_trans_with_scaling$rmse))


RMSEs["spotify_songs_cleaned_with_trans_with_scaling"] <- result_with_trans_with_scaling$rmse


####################################################################################################################

#           Bestes Ergebnis mit Dataset "spotify_songs_cleaned_with_trans" (ohne scaling)


####################################################################################################################

result_with_trans_without_scaling <- generateBaggedRegressionTree(spotify_songs_cleaned_with_trans, 0.8, "streams", "treebag", ctrl, "grid", scaling = FALSE, isTargetTransformed = TRUE )

print(paste("MSE (with transformation without & without scaling): ", result_with_trans_without_scaling$mse))
print(paste("RMSE (with transformation without & without scaling): ", result_with_trans_without_scaling$rmse))


RMSEs["spotify_songs_cleaned_with_trans_without_scaling"] <- result_with_trans_without_scaling$rmse



####################################################################################################################

#           Bestes Ergebnis mit Dataset "spotify_songs_cleaned_with_trans_optima" (mit scaling)


####################################################################################################################

result_with_trans_optima_with_scaling <- generateBaggedRegressionTree(spotify_songs_cleaned_with_trans_optima, 0.8, "streams", "treebag", ctrl, "grid", scaling = TRUE, isTargetTransformed = TRUE )

print(paste("MSE (with transformation with & with scaling): ", result_with_trans_optima_with_scaling$mse))
print(paste("RMSE (with transformation with & with scaling): ", result_with_trans_optima_with_scaling$rmse))


RMSEs["spotify_songs_cleaned_with_trans_optima_with_scaling"] <- result_with_trans_optima_with_scaling$rmse



####################################################################################################################

#           Bestes Ergebnis mit Dataset "spotify_songs_cleaned_with_trans_optima" (ohne scaling)


####################################################################################################################

result_with_trans_optima_without_scaling <- generateBaggedRegressionTree(spotify_songs_cleaned_with_trans_optima, 0.8, "streams", "treebag", ctrl, "grid", scaling = FALSE, isTargetTransformed = TRUE )

print(paste("MSE (with transformation without & without scaling): ", result_with_trans_optima_without_scaling$mse))
print(paste("RMSE (with transformation without & without scaling): ", result_with_trans_optima_without_scaling$rmse))


RMSEs["spotify_songs_cleaned_with_trans_optima_without_scaling"] <- result_with_trans_optima_without_scaling$rmse



####################################################################################################################

#           Bestes Ergebnis mit Dataset "spotify_songs_cleaned_without_trans" (mit scaling)


####################################################################################################################

result_without_trans_with_scaling <- generateBaggedRegressionTree(spotify_songs_cleaned_without_trans, 0.8, "streams", "treebag", ctrl, "grid", scaling = TRUE, isTargetTransformed = FALSE )

print(paste("MSE (with transformation with & with scaling): ", result_without_trans_with_scaling$mse))
print(paste("RMSE (with transformation with & with scaling): ", result_without_trans_with_scaling$rmse))


RMSEs["spotify_songs_cleaned_without_trans_with_scaling"] <- result_without_trans_with_scaling$rmse



####################################################################################################################

#           Bestes Ergebnis mit Dataset "spotify_songs_cleaned_without_trans" (ohne scaling)


####################################################################################################################

result_without_trans_without_scaling <- generateBaggedRegressionTree(spotify_songs_cleaned_without_trans, 0.8, "streams", "treebag", ctrl, "grid", scaling = FALSE, isTargetTransformed = FALSE )

print(paste("MSE (with transformation with & without scaling): ", result_without_trans_without_scaling$mse))
print(paste("RMSE (with transformation with & without scaling): ", result_without_trans_without_scaling$rmse))


RMSEs["spotify_songs_cleaned_without_trans_without_scaling"] <- result_without_trans_without_scaling$rmse


plottingQualityMass(RMSEs, "RMSE")


