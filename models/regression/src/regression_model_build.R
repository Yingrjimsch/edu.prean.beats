#install.packages("leaps") Install if package not installed
library(leaps)

BASE_DIR <- "./models/regression/"
DIR_DATA_CLEANING <- "./data/"
SEPARATOR <- "-------------------------------------------------------"

datasets = list(
    paste0(DIR_DATA_CLEANING, "spotify_songs_cleaned_without_trans.RData"),
    paste0(DIR_DATA_CLEANING, "spotify_songs_cleaned_with_trans.RData"),
    paste0(DIR_DATA_CLEANING, "spotify_songs_cleaned_with_trans_optima.RData")
)

process_dataset <- function(dataset_path) {

    data = get(load(dataset_path))

    # Generalize file names and paths for saving models and plots
    path_parts = unlist(strsplit(dataset_path, "/"))
    dataset_name = gsub(".RData", "", path_parts[length(path_parts)])

    print(SEPARATOR)
    print("Start processing dataset: ")
    print(dataset_name)

    # Split dataset
    set.seed(123)
    training_indices <- sample(nrow(data), 0.8 * nrow(data))
    train_data <- data[training_indices, ]
    saveRDS(training_indices, file = sprintf("%sartifacts/training_indices_%s.rds", BASE_DIR, dataset_name))


     # Initial model
    initial_model <- lm(streams ~ ., data = train_data)

    # Compute Cook's Distance
    cooksd <- cooks.distance(initial_model)

    # Identify Outliers (data points with a Cook's Distance larger than 4/n)
    outliers <- which(cooksd > (4/length(cooksd)))

    # Remove Outliers
    new_data <- train_data[-outliers, ]

    # Ensure consistency in categorical variable 'released_month'
    new_data$released_month <- factor(new_data$released_month, levels = levels(train_data$released_month))

    # Backward elimination
    backward_model <- step(lm(streams ~ ., data = new_data), direction = "backward", trace = FALSE)   

     # Forward elimination
    null_model <- lm(streams ~ 1, data = new_data)
    forward_model <- step(null_model, scope = list(lower = null_model, upper = initial_model), direction = "forward", trace = FALSE)

    # Arbitrary choice
    if (corr(train_data) < 10) {

        print("Number of correlated pairs < 10 -> Best subset selection feasible -> Start ...")

        # Best subset selection
        subset_model <- regsubsets(streams ~ ., data = new_data, nbest = 1, nvmax = NULL, method = "exhaustive", really.big = TRUE)

        # Stats
        subset_summary <- summary(subset_model)
        bic_values <- subset_summary$bic

        best_model_index <- which.min(bic_values)
        best_model_vars <- subset_summary$which[best_model_index, ]

        # Correctly filter to include only variables present in new_data
        available_vars <- names(new_data)
        best_model_vars_names <- names(best_model_vars)[best_model_vars]
        best_model_vars_names <- best_model_vars_names[best_model_vars_names %in% available_vars]

        best_model_formula <- as.formula(paste("streams ~", paste(best_model_vars_names, collapse = " + ")))
        final_model <- lm(best_model_formula, data = new_data)

    } else {

        print("Number of correlated pairs < 10 -> Best subset selection infeasible -> skip")

        final_model <- NULL
        bic_values <- NULL
        best_model_vars_names <- NULL

    }

    print("Dataset processed!")

    return(list(
        name = dataset_name,
        models = list(
            initial_model = initial_model, 
            backward_model = backward_model, 
            forward_model = forward_model, 
            final_model = final_model
        ),
        stats = list(
            cooksd = cooksd,
            outliers = outliers,
            bic_values = bic_values,
            best_model_vars_names = best_model_vars_names
        )
    ))

}

save <- function(data) {

    DATASET_NAME = data$name

    # Initial model
    saveRDS(data$models$initial_model, sprintf("%sartifacts/model_initial_%s.rds", BASE_DIR, DATASET_NAME))
    png(sprintf("%simg/residuals_initial_model_%s.png", BASE_DIR, DATASET_NAME))
    plot(residuals(data$models$initial_model), main = "Residuals of Initial Model")
    dev.off()

    # Cooks distance
    png(sprintf("%simg/cook_%s.png", BASE_DIR, DATASET_NAME))
    plot(data$stats$cooksd, pch = "*", cex = 2, main = "Cook's Distance")
    abline(h = 4/length(data$stats$cooksd), col = "red")
    dev.off()

    # Backward model
    saveRDS(data$models$backward_model, sprintf("%sartifacts/model_backward_%s.rds", BASE_DIR, DATASET_NAME))
    png(sprintf("%simg/residuals_backward_model_%s.png", BASE_DIR, DATASET_NAME))
    plot(residuals(data$models$backward_model), main = "Residuals of Backward Model")
    dev.off()

    # Forward model
    saveRDS(data$models$forward_model, sprintf("%sartifacts/model_forward_%s.rds", BASE_DIR, DATASET_NAME))
    png(sprintf("%simg/residuals_forward_model_%s.png", BASE_DIR, DATASET_NAME))
    plot(residuals(data$models$forward_model), main = "Residuals of Forward Model")
    dev.off()

    if (!is.null(data$models$final_model)) {
        saveRDS(data$models$final_model, sprintf("%sartifacts/model_final_%s.rds", BASE_DIR, DATASET_NAME))
        png(sprintf("%simg/residuals_final_model_%s.png", BASE_DIR, DATASET_NAME))
        plot(residuals(data$models$final_model), main = "Residuals of Final Model")
        dev.off()
    }

}

log <- function(data) {

    log_file_name <- sprintf("%slogs/log_%s.txt", BASE_DIR, data$name)
    sink(log_file_name)

    print("------------- INITIAL MODEL (ALL FEATURES) --------------")
    print(summary(data$models$initial_model))
    print(SEPARATOR)

    print("------------- NUMBER OF REMOVED OUTLIERS --------------")
    print(length(data$stats$outliers))
    print(SEPARATOR)

    print("------------- BACKWARD MODEL --------------")
    print(summary(data$models$backward_model))
    print(SEPARATOR)

    print("------------- FORWARD MODEL --------------")
    print(summary(data$models$forward_model))
    print(SEPARATOR)

    if (!is.null(data$models$final_model)) {
        
        print("------------- SUBSET SELECTION --------------")

        print("BIC VALUES:")
        print(data$stats$bic_values)
        print("BEST MODEL VARIABLES:")
        print(data$stats$best_model_vars_names)
        print(SEPARATOR)

        print("------------- FINAL MODEL --------------")
        print(summary(data$models$final_model))
        print(SEPARATOR)

    }

    sink()

}


corr <- function(data) {

    numeric_data <- data[, sapply(data, is.numeric)]
    variable_names <- colnames(numeric_data)
    cor_matrix <- cor(numeric_data, use = "complete.obs")

    # Set threshold for high correlation
    threshold <- 0.8

    # Find highly correlated pairs
    high_corr_pairs <- which(abs(cor_matrix) > threshold, arr.ind = TRUE)

    number_of_pairs <- nrow(high_corr_pairs) / 2

    # Iterate over each pair and print correlation
    for (i in 1:nrow(high_corr_pairs)) {
        # Extract row and column indices
        row_idx <- high_corr_pairs[i, "row"]
        col_idx <- high_corr_pairs[i, "col"]

        # Get variable names
        var1 <- variable_names[row_idx]
        var2 <- variable_names[col_idx]

        # Print variable names and their correlation
        # cat(var1, "and", var2, ":", cor_matrix[row_idx, col_idx], "\n")
    }


    return(number_of_pairs)

} 

for (dataset_path in datasets) {

    results = process_dataset(dataset_path)

    log(results)
    save(results)
    
}