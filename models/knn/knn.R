# Install packages if not available
#install.packages("class")
#install.packages("Metrics")
#install.packages("kknn")
#install.packages("caret")
library(class)
library(Metrics)
library(kknn)
library(caret)
load("../../data/spotify_songs_cleaned_without_trans.RData")
load("../../data/spotify_songs_cleaned_with_trans.RData")
load("../../data/spotify_songs_cleaned_with_trans_optima.RData")

str(spotify_songs_cleaned_without_trans)

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

set.seed(69)  #für die Reproduzierbarkeit

exec_knn <- function(data) {
  
  folder_name <- "./img"
  
  if (!dir.exists(folder_name)) {
    
    dir.create(folder_name)
    
  }
  
  file_name <- deparse(substitute(data))
  
  path_name <-paste0(folder_name, "/", file_name )
  
  sample_indices <- sample(nrow(data), 0.8 * nrow(data))
  
  # 80% of data for training
  train_data <- data[sample_indices, ]
  
  # 20% of data for testing
  test_data <- data[-sample_indices, ]
  
  # Assuming the last column is the target variable, and other columns are features
  # Adjust the column indices accordingly
  features <- train_data[, -ncol(train_data)]
  labels <- train_data[, ncol(train_data)]
  
  ctrl <- trainControl(method = "cv", number = 5)
  k_values <- seq(1, 20, by = 1)
  tuneGrid <- expand.grid(kmax = k_values, distance = 1:3, kernel = c("gaussian", "optimal"))
  
  knn_model <- train(
    x = features,  # Features (exclude target variable)
    y = labels,   # Target variable
    method = "kknn",
    tuneGrid = tuneGrid,
    trControl = ctrl
  )
  
  print(knn_model)
  
  # Access the best k value
  best_k <- knn_model$bestTune$kmax
  cat("Best k value:", best_k, "\n")
  
    
  # Perform KNN
  knn_predictions <- predict(kknn(streams ~ ., train_data, test_data, k = best_k))
  
  observed_vs_predicted(path_name, test_data$streams, knn_predictions)
  
  mse <- mean(test_data$streams - knn_predictions)^2
  rmse <- sqrt(mse)
  mae <- mae(test_data$streams, knn_predictions)
  
  cat("Mean Squared Error:", mse, "\n")
  cat("Root Mean Sbsolute Error:", rmse, "\n")
  cat("Mean Absolute Error:", mae, "\n")
  
  return(list(knn_model = knn_model, mae = mae, mse = mse, rmse = rmse))
}

results_without_trans <- exec_knn(spotify_songs_cleaned_without_trans)
# Model für spätere Verwendung im Dataproduct
knn_model <- results_without_trans$knn_model
writeLines(capture.output(summary(knn_model)), "../../data/RDataModels/knn/knn_model_summary.txt")
saveRDS(knn_model, "../../data/RDataModels/knn/knn_model.rds")
saveRDS(results_without_trans, "../../data/RDataModels/knn/knn_model_results.rds")


results_with_trans <- exec_knn(spotify_songs_cleaned_with_trans)

results_with_trans_optima <- exec_knn(spotify_songs_cleaned_with_trans_optima)
# Model für spätere Verwendung im Dataproduct
knn_model_trans <- results_with_trans_optima$knn_model
writeLines(capture.output(summary(knn_model_trans)), "../../data/RDataModels/knn/knn_model_trans_summary.txt")
saveRDS(knn_model_trans, "../../data/RDataModels/knn/knn_model_trans.rds")
saveRDS(results_with_trans_optima, "../../data/RDataModels/knn/knn_model_trans_results.rds")

