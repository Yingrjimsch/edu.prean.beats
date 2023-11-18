load("./edu.prean.beats/Datenbereinigung/spotify_songs_cleaned_with_trans_optima.RData")

data = spotify_songs_cleaned_with_trans_optima

model <- lm(streams ~ ., data = data)

print("------------- INITIAL MODEL (ALL FEATURES) --------------")
print(summary(model))

# Compute Cook's Distance
cooksd <- cooks.distance(model)

# Identify Outliers (data points with a Cook's Distance larger than 4/n)
outliers <- which(cooksd > (4/length(cooksd)))

print("------------- NUMBER OF REMOVED OUTLIERS DUE TO COOK'S DISTANCE --------------")
print(length(outliers))

png("./edu.prean.beats/models/regression/img/regWithTransOpt_Cook.png")
plot(cooksd, pch = "*", cex = 2, main = "Cook's Distance")
abline(h = 4/length(cooksd), col = "red")
dev.off()

# Remove Outliers
new_data <- data[-outliers, ]

# Ensure consistency in categorical variable 'released_month'
new_data$released_month <- factor(new_data$released_month, levels = levels(data$released_month))

# Backward elimination
backward_model <- step(lm(streams ~ ., data = new_data), direction = "backward", trace = FALSE)

print("------------- BACKWARD MODEL --------------")
print(summary(backward_model))

# Forward elimination
null_model <- lm(streams ~ 1, data = new_data)
forward_model <- step(null_model, scope = list(lower = null_model, upper = model), direction = "forward", trace = FALSE)

print("------------- FORWARD MODEL --------------")
print(summary(forward_model))

# Best subset selection
library(leaps)
subset_model <- regsubsets(streams ~ ., data = new_data, nbest = 1, nvmax = NULL, method = "exhaustive")

subset_summary <- summary(subset_model)
bic_values <- subset_summary$bic

print("------------- SUBSET SELECTION --------------")
print("BIC VALUES:")
print(bic_values)

best_model_index <- which.min(bic_values)
best_model_vars <- subset_summary$which[best_model_index, ]

# Correctly filter to include only variables present in new_data
available_vars <- names(new_data)
best_model_vars_names <- names(best_model_vars)[best_model_vars]
best_model_vars_names <- best_model_vars_names[best_model_vars_names %in% available_vars]

print("BEST MODEL VARIABLES:")
print(best_model_vars_names)

best_model_formula <- as.formula(paste("streams ~", paste(best_model_vars_names, collapse = " + ")))
final_model <- lm(best_model_formula, data = new_data)

print("------------- FINAL MODEL --------------")
print(summary(final_model))

# Residual Plot
png("./edu.prean.beats/models/regression/img/regWithTransOpt_Residuals.png")
plot(residuals(final_model), main = "Residuals of Final Model")
dev.off()