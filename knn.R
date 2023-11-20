install.packages("class")
install.packages("Metrics")
install.packages("kknn")
install.packages("caret")
library(class)
library(Metrics)
library(kknn)
library(caret)
load("spotify_songs_cleaned_without_trans.RData")
load("spotify_songs_cleaned_with_trans.RData")
load("spotify_songs_cleaned_with_trans_optima.RData")

str(spotify_songs_cleaned_without_trans)

set.seed(69)  # für die Reproduzierbarkeit

exec_knn <- function(data) {
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
  
  mse <- mean(test_data$streams - knn_predictions)^2
  rmse <- sqrt(mse)
  mae <- mae(test_data$streams, knn_predictions)
  
  cat("Mean Squared Error:", mse, "\n")
  cat("Root Mean Sbsolute Error:", rmse, "\n")
  cat("Mean Absolute Error:", mae, "\n")
}

exec_knn(spotify_songs_cleaned_without_trans)
exec_knn(spotify_songs_cleaned_with_trans)
exec_knn(spotify_songs_cleaned_with_trans_optima)











install.packages("keras")
install.packages("tensorflow")
library(magrittr)
library(keras)
library(tensorflow)
library(dplyr)



library(reticulate)
use_condaenv(condaenv = "base", required = TRUE)

set.seed(69)  # für die Reproduzierbarkeit
sample_indices <- sample(nrow(spotify_songs_cleaned_with_trans_optima), 0.8 * nrow(spotify_songs_cleaned_with_trans_optima))

# 80% of data for training
train_data <- spotify_songs_cleaned_with_trans_optima[sample_indices, ]

# 20% of data for testing
test_data <- spotify_songs_cleaned_with_trans_optima[-sample_indices, ]

# Assuming the last column is the target variable, and other columns are features
# Adjust the column indices accordingly
features <- train_data[, -ncol(train_data)]
labels <- train_data[, ncol(train_data)]
test_features <- test_data[, -ncol(test_data)]
test_labels <- test_data[, ncol(test_data)]

model <- keras_model_sequential() %>%
  layer_dense(units = 64, input_shape = 20, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1)  # Output layer for regression

model %>% compile(
  optimizer = "adam",
  loss = "mean_squared_error"
)

model %>% fit(
  x = as.matrix(features]),  # Features (excluding target column)
  y = train_data[, labels],  # Target variable
  epochs = 50,
  batch_size = 32,
  validation_split = 0.2
)

predictions <- model %>% predict(as.matrix(test_features))
mse <- mean((predictions - test_labels)^2)
mae <- mean(abs(predictions - test_labels))

cat("Mean Squared Error:", mse, "\n")
cat("Mean Absolute Error:", mae, "\n")