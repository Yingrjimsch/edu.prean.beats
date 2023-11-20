library(leaps)

# Define directories
BASE_DIR <- "./edu.prean.beats/models/regression/"
DIR_DATA_CLEANING <- "./edu.prean.beats/Datenbereinigung/"

# List of datasets and corresponding training index files
datasets <- list(
    list(
        data = paste0(DIR_DATA_CLEANING, "spotify_songs_cleaned_without_trans.RData"),
        indices = paste0(BASE_DIR, "artifacts/training_indices_spotify_songs_cleaned_without_trans.rds")
    ),
    list(
        data = paste0(DIR_DATA_CLEANING, "spotify_songs_cleaned_with_trans.RData"),
        indices = paste0(BASE_DIR, "artifacts/training_indices_spotify_songs_cleaned_with_trans.rds")
    ),
    list(
        data = paste0(DIR_DATA_CLEANING, "spotify_songs_cleaned_with_trans_optima.RData"),
        indices = paste0(BASE_DIR, "artifacts/training_indices_spotify_songs_cleaned_with_trans_optima.rds")
    )
)

test_models <- function(dataset, indices_file) {
    
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

            # Calculate evaluation metrics
            rmse <- sqrt(mean((test_data$streams - predictions)^2))
            mae <- mean(abs(test_data$streams - predictions))
            mape <- mean(abs((test_data$streams - predictions) / test_data$streams)) * 100
            mse <- mean((test_data$streams - predictions)^2)
            r_squared <- summary(model)$r.squared

            n <- nrow(test_data) # Number of observations
            p <- length(coefficients(model)) - 1 # Number of predictors
            adj_r_squared <- 1 - (1 - r_squared) * ((n - 1) / (n - p - 1))

            f_statistic <- summary(model)$fstatistic[1]
            aic_value <- AIC(model)
            bic_value <- BIC(model)

            # Print evaluation metrics
            cat(sprintf("Model: %s\nRMSE: %f\nMAE: %f\nMAPE: %f\nMSE: %f\nR-squared: %f\nAdjusted R-squared: %f\nF-statistic: %f\nAIC: %f\nBIC: %f\n\n",
                        model_type, rmse, mae, mape, mse, r_squared, adj_r_squared, f_statistic, aic_value, bic_value))
        }
    }
}

# Test each dataset
for (dataset in datasets) {
    cat("Testing dataset:", dataset$data, "\n")
    test_models(dataset, dataset$indices)
}