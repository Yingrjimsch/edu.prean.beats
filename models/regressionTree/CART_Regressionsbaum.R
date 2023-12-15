##############################################################################


#                           CART Regressionsbaum


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


generateHyperGrid <- function(minSplitSequence, maxDepthSequence) {
  hyper_grid <- expand.grid(
    minsplit = minSplitSequence, # e. g seq(5, 20, 1)
    maxdepth = maxDepthSequence # e. g. seq(8, 15, 1) 
  )
  return (hyper_grid = hyper_grid)
}


# Erstellen einen Grids von Trees für späteren Grid Search

generateTreesForGridSearch <- function(trainData, minSplitSequence, maxDepthSequence) {
  trees <- list()
  hyper_grid <- generateHyperGrid(minSplitSequence, maxDepthSequence)
  for (i in 1:nrow(hyper_grid)) {
    
  
    minsplit <- hyper_grid$minsplit[i]
    maxdepth <- hyper_grid$maxdepth[i]
    
    # trainieren des Models 
    trees[[i]] <- rpart(
      formula = streams ~ .,
      data    = trainData,
      method  = "anova",
      control = list(minsplit = minsplit, maxdepth = maxdepth)
    )
  }
  return(list(hyper_grid = hyper_grid, trees = trees)) 
}

# besten cp erhalten
get_cp <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  cp <- x$cptable[min, "CP"] 
}

# minimaler Fehler erhalten
get_min_error <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"] 
}

# aus allen verwndeten Bäumen die n- besten extrahieren
getTopNTrees <- function(hyper_grid, trees) {
  hyper_grid %>%
    mutate(
      cp    = purrr::map_dbl(trees, get_cp),
      error = purrr::map_dbl(trees, get_min_error)
    ) %>%
    arrange(error) %>%
    top_n(-5, wt = error)
}

# Rücktransformation der mittels Logarithmus transformierten Vorhersagen und akteulle Werte
backtransformation <- function(pred, actual){
  
  log_pred <- pred 
  log_actual <- actual  
  
  pred <- exp(log_pred)
  actual <- exp(log_actual)
  return (list(pred = pred, actual = actual))
  
}

# Plot observed vs predicted values
observed_vs_predicted <- function(path_name, actual, pred) {
  full_filename <- paste0(path_name, "_observed_vs_predicted.png")
  png(file = full_filename, width = 800, height = 600)
  plot <- ggplot(data = NULL, aes(x = actual, y = pred)) +
    geom_text(label="★", color="orange", size=3) +  # Fügt die Punkte hinzu
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue") + # Fügt eine y=x Linie hinzu
    labs(x = "Observed", y = "Predicted", title = "Observed vs Predicted Plot") +
    theme_minimal()
  print(plot)
  # Schließen der PNG-Aufzeichnung
  dev.off()
}

#Erstellen von Plots der Gütemasse (auskommentiert, da Werte zu hoch und dadurch deren Unterschiede in einem Diagramm kaum ersichtlich sind)
plottingQualityMass <- function(qualityVector, value, savePath) {
  color <- brewer.pal(6, "PRGn")
  # Umwandeln der Liste in einen Dataframe
  qualityDf <- data.frame(Model = names(qualityVector), value = qualityVector)

  # Erstellen des Plots
  plot <- ggplot(qualityDf, aes(x = Model, y = value, fill = Model)) +
    geom_bar(stat = "identity", width = 0.7) +
    scale_fill_manual(values = color) +
    theme_minimal() +
    theme(axis.text.x = element_blank()) +
    labs(title = paste("Vergleich der", value, "-Werte verschiedener Modelle"),
         x = "Modell",
         y = value)

  # Speichern des Plots als PNG
  ggsave(filename = paste0("img/quality_comparison_", value, ".png"),
         plot = plot,
         device = "png",
         width = 10,
         height = 6,
         units = "in")
}



################################ Main Funktion ################################


# Erstellen von Plots je nach Prediktor (numerisch oder kategoriell) und speichern als png
generateOptimalRegressionTree <- function(dataframe, splitfactor, target_var, scaling = TRUE, isTargetTransformed = TRUE, minSplitSequence, maxDepthSequence) {
  set.seed(123)
  folder_name <- "./img"
  
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
  
  
  formula <- as.formula(paste(target_var, "~."))
  
  base_tree <- rpart(
    formula = formula,
    data    = trainData,
    method  = "anova"
  )

  print("#######################  Base Tree ###########################")
  print(base_tree)

  print("####################### Plot Base Tree ###################")
  full_filename <- paste0(path_name, "_base_tree.png")
  png(file = full_filename, width = 800, height = 600)
  rpart.plot(base_tree, main = paste0(file_name, "_base_tree"))
  dev.off()
  print("####################### cp Table Base Tree ###################")
  print(base_tree$cptable)


  print("####################### Plot cp Base Tree ###################")
  full_filename <- paste0(path_name, "_cptable_base_tree.png")
  png(file = full_filename, width = 800, height = 600)
  plotcp(base_tree, main = "")
  title(main = paste0(file_name, "_cptable_base_tree"), line = -1) 


  full_tree <- rpart(
    formula = formula,
    data    = trainData,
    method  = "anova",
    control = list(cp = 0, xval = 10)
  )
  dev.off()
  print("#######################  Full Tree ###########################")
  #print(full_tree)

  print("####################### cp Table Full Tree ###################")
  print(full_tree$cptable)

  print("####################### Plot cp Full Tree ###################")
  full_filename <- paste0(path_name, "_cptable_full_tree.png")
  png(file = full_filename, width = 800, height = 600)
  plotcp(full_tree, main = "")
  title(main = paste0(file_name, "_cptable_full_tree"), line = -1) 
  dev.off()

  results <- generateTreesForGridSearch(trainData, minSplitSequence, maxDepthSequence)
  hyper_grid <- results$hyper_grid
  trees <- results$trees


  topNTrees <- getTopNTrees(hyper_grid, trees)
  topNTrees <- topNTrees %>% arrange(error)
  
 
  print("####################### Top five Trees #######################")
  print(topNTrees)

  best_result <- topNTrees[1, ]
  best_minSplit <- best_result$minsplit
  best_maxdepth <- best_result$maxdepth
  best_cp <- best_result$cp

  optimal_tree <- rpart(
    formula = formula,
    data    = trainData,
    method  = "anova",
    control = list(minsplit = best_minSplit, maxdepth =  best_maxdepth, cp = best_cp)
  )
  print("################# TrainData Target ##################")
  trainData[[target_var]]

  print("#######################  Optimal Tree ###########################")
  print(optimal_tree)

  print("####################### Plot Optimal Tree ###################")
  full_filename <- paste0(path_name, "_optimal_tree.png")
  png(file = full_filename, width = 800, height = 600)
  rpart.plot(optimal_tree, main = paste0(file_name, "_optimal_tree"))

  dev.off()
  
  pred <- predict(optimal_tree, newdata = testData)
  actual <- testData[[target_var]]
  observed_vs_predicted(path_name, actual, pred)

  if(isTargetTransformed){
    backtrans <- backtransformation(pred, actual)
    pred <- backtrans$pred
    actual <- backtrans$actual
  }
  # Berechnung des MAE, MSE und RMSE
  mae <- mean(abs(pred - actual))
  mse <- mean((pred - actual)^2)
  rmse <- sqrt(mse)
  

  return(list(optimal_tree = optimal_tree, minSplit = best_minSplit, maxdepth = best_maxdepth, cp = best_cp, mae = mae, mse = mse, rmse = rmse))
}

###################################################################################################################

# Anwenden an alle Dataframes


###################################################################################################################

RMSEs <- c() # Vektor um alle RMSE's der Modelle abzulegen
MSEs <-c()

####################################################################################################################

#           Bestes Ergebnis mit Dataset "spotify_songs_cleaned_with_trans" (mit scaling)


####################################################################################################################

result_with_trans_with_scaling <- generateOptimalRegressionTree(spotify_songs_cleaned_with_trans, 0.80, "streams", scaling = TRUE, isTargetTransformed = TRUE, minSplitSequence = seq(20, 100, 10), maxDepthSequence = seq(4, 15, 1))



print(paste("cp (with transformation & with scaling): ", result_with_trans_with_scaling$cp))
print(paste("minsplit (with transformation & with scaling): ", result_with_trans_with_scaling$minSplit))
print(paste("maxdepth (with transformation & with scaling): ", result_with_trans_with_scaling$maxdepth))
print(paste("MAE (with transformation with & scaling): ", result_with_trans_with_scaling$mae))
print(paste("MSE (with transformation with & scaling): ", result_with_trans_with_scaling$mse))
print(paste("RMSE (with transformation with & scaling): ", result_with_trans_with_scaling$rmse))

MSEs["spotify_songs_cleaned_with_trans_with_scaling"] <- result_with_trans_with_scaling$mse
RMSEs["spotify_songs_cleaned_with_trans_with_scaling"] <- result_with_trans_with_scaling$rmse

####################################################################################################################

#           Bestes Ergebnis mit Dataset "spotify_songs_cleaned_with_trans" (ohne scaling)


####################################################################################################################


result_with_trans_without_scaling <- generateOptimalRegressionTree(spotify_songs_cleaned_with_trans, 0.80, "streams", scaling = FALSE, isTargetTransformed = TRUE, minSplitSequence = seq(20, 100, 10), maxDepthSequence = seq(4, 15, 1))

print(paste("cp (with transformation & without scaling): ", result_with_trans_without_scaling$cp))
print(paste("minsplit (with transformation & without scaling): ", result_with_trans_without_scaling$minSplit))
print(paste("maxdepth (with transformation & without scaling): ", result_with_trans_without_scaling$maxdepth))
print(paste("MAE (with transformation & without scaling): ", result_with_trans_without_scaling$mae))
print(paste("MSE (with transformation & without scaling): ", result_with_trans_without_scaling$mse))
print(paste("RMSE (with transformation & without scaling): ", result_with_trans_without_scaling$rmse))

MSEs["spotify_songs_cleaned_with_trans_without_scaling"] <- result_with_trans_without_scaling$mse
RMSEs["spotify_songs_cleaned_with_trans_without_scaling"] <- result_with_trans_without_scaling$rmse

####################################################################################################################

#           Bestes Ergebnis mit Dataset "spotify_songs_cleaned_with_trans_optima" (mit scaling)


####################################################################################################################


result_with_trans_optima_with_scaling <- generateOptimalRegressionTree(spotify_songs_cleaned_with_trans_optima, 0.80, "streams", scaling = TRUE, isTargetTransformed = TRUE, minSplitSequence = seq(20, 100, 10), maxDepthSequence = seq(4, 15, 1))

# Model für spätere Verwendung im Dataproduct
rt_model_trans <- result_with_trans_optima_with_scaling$optimal_tree
writeLines(capture.output(summary(rt_model_trans)), "../../data/RDataModels/regressionTree/rt_model_trans_summary.txt")
saveRDS(rt_model_trans, "../../data/RDataModels/regressionTree/rt_model_trans.rds")
saveRDS(result_with_trans_optima_with_scaling, "../../data/RDataModels/regressionTree/rt_model_trans_results.rds")

print(paste("cp (with transformation & with scaling): ", result_with_trans_optima_with_scaling$cp))
print(paste("minsplit (with transformation & with scaling): ", result_with_trans_optima_with_scaling$minSplit))
print(paste("maxdepth (with transformation & with scaling): ", result_with_trans_optima_with_scaling$maxdepth))
print(paste("MAE (with transformation & with scaling): ", result_with_trans_optima_with_scaling$mae))
print(paste("MSE (with transformation & with scaling): ", result_with_trans_optima_with_scaling$mse))
print(paste("RMSE (with transformation & with scaling): ", result_with_trans_optima_with_scaling$rmse))

MSEs["spotify_songs_cleaned_with_trans_optima_with_scaling"] <- result_with_trans_optima_with_scaling$mse
RMSEs["spotify_songs_cleaned_with_trans_optima_with_scaling"] <- result_with_trans_optima_with_scaling$rmse

####################################################################################################################

#           Bestes Ergebnis mit Dataset "spotify_songs_cleaned_with_trans_optima" (ohne scaling)


####################################################################################################################


result_with_trans_optima_without_scaling <- generateOptimalRegressionTree(spotify_songs_cleaned_with_trans_optima, 0.80, "streams", scaling = FALSE, isTargetTransformed = TRUE, minSplitSequence = seq(20, 100, 10), maxDepthSequence = seq(4, 15, 1))


print(paste("cp (with transformation & without scaling): ", result_with_trans_optima_without_scaling$cp))
print(paste("minsplit (with transformation &  without scaling): ", result_with_trans_optima_without_scaling$minSplit))
print(paste("maxdepth (with transformation & without scaling): ", result_with_trans_optima_without_scaling$maxdepth))
print(paste("MAE (with transformation & without scaling): ", result_with_trans_optima_without_scaling$mae))
print(paste("MSE (with transformation & without scaling): ", result_with_trans_optima_without_scaling$mse))
print(paste("RMSE (with transformation &  without scaling): ", result_with_trans_optima_without_scaling$rmse))

MSEs["spotify_songs_cleaned_with_trans_optima_without_scaling"] <- result_with_trans_optima_without_scaling$mse
RMSEs["spotify_songs_cleaned_with_trans_optima_without_scaling"] <- result_with_trans_optima_without_scaling$rmse


####################################################################################################################

#           Bestes Ergebnis mit Dataset "spotify_songs_cleaned_without_trans" (mit scaling)


####################################################################################################################


result_without_trans_with_scaling <- generateOptimalRegressionTree(spotify_songs_cleaned_without_trans, 0.80, "streams", scaling = TRUE, isTargetTransformed = FALSE, minSplitSequence = seq(20, 100, 10), maxDepthSequence = seq(4, 15, 1))

# Model für spätere Verwendung im Dataproduct
rt_model <- result_without_trans_with_scaling$optimal_tree
writeLines(capture.output(summary(rt_model)), "../../data/RDataModels/regressionTree/rt_model_summary.txt")
saveRDS(rt_model, "../../data/RDataModels/regressionTree/rt_model.rds")
saveRDS(result_without_trans_with_scaling, "../../data/RDataModels/regressionTree/rt_model_results.rds")

print(paste("cp (without transformation & with scaling): ", result_without_trans_with_scaling$cp))
print(paste("minsplit (without transformation &  with scaling): ", result_without_trans_with_scaling$minSplit))
print(paste("maxdepth (without transformation & with scaling): ", result_without_trans_with_scaling$maxdepth))
print(paste("MAE (without transformation & with scaling): ", result_without_trans_with_scaling$mae))
print(paste("MSE (without transformation & with scaling): ", result_without_trans_with_scaling$mse))
print(paste("RMSE (without transformation &  with scaling): ", result_without_trans_with_scaling$rmse))


MSEs["spotify_songs_cleaned_without_trans_with_scaling"] <- result_without_trans_with_scaling$mse
RMSEs["spotify_songs_cleaned_without_trans_with_scaling"] <- result_without_trans_with_scaling$rmse

####################################################################################################################

#           Bestes Ergebnis mit Dataset "spotify_songs_cleaned_without_trans" (ohne scaling)


####################################################################################################################


result_without_trans_without_scaling <- generateOptimalRegressionTree(spotify_songs_cleaned_without_trans, 0.80, "streams", scaling = FALSE, isTargetTransformed = FALSE, minSplitSequence = seq(20, 100, 10), maxDepthSequence = seq(4, 15, 1))

print(paste("cp (without transformation & without scaling): ", result_without_trans_without_scaling$cp))
print(paste("minsplit (without transformation &  without scaling): ", result_without_trans_without_scaling$minSplit))
print(paste("maxdepth (without transformation & without scaling): ", result_without_trans_without_scaling$maxdepth))
print(paste("MAE (without transformation & without scaling): ", result_without_trans_without_scaling$mae))
print(paste("MSE (without transformation & without scaling): ", result_without_trans_without_scaling$mse))
print(paste("RMSE (without transformation &  without scaling): ", result_without_trans_without_scaling$rmse))

MSEs["spotify_songs_cleaned_without_trans_without_scaling"] <- result_without_trans_without_scaling$mse
RMSEs["spotify_songs_cleaned_without_trans_scaling"] <- result_without_trans_without_scaling$rmse


plottingQualityMass(MSEs, "MSE")

plottingQualityMass(RMSEs, "RMSE")


