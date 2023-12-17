# install.packages("leaps") Install if package not installed
# install.packages("ggplot2")
library("leaps")
library("ggplot2")

# Define directories
BASE_DIR <- "./models/regression/"
DIR_DATA_CLEANING <- "./data/"

# List of datasets and corresponding training index files
datasets <- list(
    list(
        data = paste0(DIR_DATA_CLEANING, "spotify_songs_cleaned_without_trans.RData"),
        indices = paste0(BASE_DIR, "artifacts/training_indices_spotify_songs_cleaned_without_trans.rds"),
        isTransformed = FALSE
    ),
    list(
        data = paste0(DIR_DATA_CLEANING, "spotify_songs_cleaned_with_trans.RData"),
        indices = paste0(BASE_DIR, "artifacts/training_indices_spotify_songs_cleaned_with_trans.rds"),
        isTransformed = TRUE
    ),
    list(
        data = paste0(DIR_DATA_CLEANING, "spotify_songs_cleaned_with_trans_optima.RData"),
        indices = paste0(BASE_DIR, "artifacts/training_indices_spotify_songs_cleaned_with_trans_optima.rds"),
        isTransformed = TRUE
    )
)

test_models <- function(dataset, indices_file, isTransformed) {
    
    data <- get(load(dataset$data))
    training_indices <- readRDS(indices_file)
    
    # Split data into training and test sets
    test_data <- data[-training_indices, ]

    # List of model types
    model_types <- c("initial", "backward", "forward", "final")

    # Iterate over each model and evaluate
    for (model_type in model_types) {
        model_file <- sprintf("%sartifacts/model_%s_%s.rds", BASE_DIR, model_type, basename(gsub(".RData", "", dataset$data)))
        
        # Check if model file exists
        if (file.exists(model_file)) {

            model <- readRDS(model_file)

            # Predict on test data
            predictions <- predict(model, newdata = test_data)

            # Backtransformation
            if (isTransformed) {
                backtrans <- backtransformation(predictions, test_data$streams)
                pred <- backtrans$pred
                actual <- backtrans$actual
            } else {
                pred <- predictions
                actual <- test_data$streams
            }

            # Calculate evaluation metrics
            rmse <- sqrt(mean((actual - pred)^2))
            mae <- mean(abs(actual - pred))
            mape <- mean(abs((actual - pred) / actual)) * 100
            mse <- mean((actual - pred)^2)
            r_squared <- summary(model)$r.squared

            n <- nrow(test_data) # Number of observations
            p <- length(coefficients(model)) - 1 # Number of predictors
            adj_r_squared <- 1 - (1 - r_squared) * ((n - 1) / (n - p - 1))

            f_statistic <- summary(model)$fstatistic[1]
            aic_value <- AIC(model)
            bic_value <- BIC(model)

            observed_vs_predicted(sprintf("%simg/%s_%s", BASE_DIR, model_type, basename(gsub(".RData", "", dataset$data))), pred, actual)

            # Print evaluation metrics
            cat(sprintf("Model: %s\nRMSE: %f\nMAE: %f\nMAPE: %f\nMSE: %f\nR-squared: %f\nAdjusted R-squared: %f\nF-statistic: %f\nAIC: %f\nBIC: %f\n\n",
                        model_type, rmse, mae, mape, mse, r_squared, adj_r_squared, f_statistic, aic_value, bic_value))
        
            saveRDS(list(regression = predictions, mae = mae, mse = mse, rmse = rmse), sprintf("%sartifacts/results_%s_%s.rds", BASE_DIR, model_type, basename(gsub(".RData", "", dataset$data))))
        }
    }
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
  full_filename <- paste0(path_name, "_regression_observed_vs_predicted.png")
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


# Test each dataset
for (dataset in datasets) {
    cat("Testing dataset:", dataset$data, "\n")
    test_models(dataset, dataset$indices, dataset$isTransformed)
}