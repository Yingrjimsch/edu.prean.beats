
library(leaps)
library(ggplot2)
library(dplyr)


BASE_DIR <- "./edu.prean.beats/models/regression/"
DIR_DATA_CLEANING <- "./edu.prean.beats/Datenbereinigung/"

datasets = list(
    paste0(DIR_DATA_CLEANING, "spotify_songs_cleaned_without_trans.RData"),
    paste0(DIR_DATA_CLEANING, "spotify_songs_cleaned_with_trans.RData"),
    paste0(DIR_DATA_CLEANING, "spotify_songs_cleaned_with_trans_optima.RData")
)

data = get(load(paste0(DIR_DATA_CLEANING, "spotify_songs_cleaned_with_trans.RData")))

 # Initial model
initial_model <- lm(streams ~ ., data = data)

# Compute Cook's Distance
cooksd <- cooks.distance(initial_model)

# Identify Outliers (data points with a Cook's Distance larger than 4/n)
outliers <- which(cooksd > (4/length(cooksd)))

# Extracting outlier data
outlier_data <- data[outliers,]

summary = summary(data)

print(summary)

# Write the outlier data to a CSV file for detailed inspection
# write.csv(outlier_data, "./edu.prean.beats/models/regression/outliers_with_trans_optima.csv", row.names = FALSE)