
# Change path according to your system settings
data <- read.csv("./edu.prean.beats/Datenbereinigung/spotify-2023_cleaned.csv")

# features1 focuses on the intrinsic musical qualities of the tracks. 
# These features are commonly associated with the auditory and emotional 
# characteristics of music, which can influence listener preferences and 
# streaming behavior. For instance, danceability, valence, and energy levels 
# might be crucial in determining a song's appeal to a broad audience.
features1 <- c("streams", "bpm", "key", "mode", "danceability_.", "valence_.", "energy_.",
              "acousticness_.", "liveness_.", "speechiness_.")

# features2 emphasize the track's presence and performance across various music platforms. 
# The inclusion in charts and playlists can significantly impact a song's streams, as it increases 
# visibility and accessibility to a wider audience. This set will help in understanding how platform 
# presence correlates with streaming numbers.
features2 <- c("streams", "in_spotify_playlists", "in_spotify_charts", "in_apple_playlists", 
               "in_apple_charts", "in_deezer_playlists", "in_shazam_charts")   # "in_deezer_charts" follow when dataset is cleaned from inf values 

# features3 is a mix of intrinsic musical attributes and platform performance metrics. It aims to 
# capture both the artistic elements of the tracks and their commercial success across platforms. 
# This combination can provide a more comprehensive view of what drives a song's popularity, blending 
# musicality with market performance.
features3 <- c("streams", "bpm", "danceability_.", "energy_.", "acousticness_.", "in_spotify_playlists", 
               "in_spotify_charts", "in_apple_charts") # "in_deezer_charts" follow when dataset is cleaned from inf values 

reg_data <- data[features3]

# Split the data into training and testing sets
set.seed(123) # for reproducibility
training_indices <- sample(nrow(reg_data), 0.8 * nrow(reg_data))
train_data <- reg_data[training_indices, ]
test_data <- reg_data[-training_indices, ]

# Fit model
model <- lm(streams ~ ., data = train_data)

print(summary(model))

# Predict on test data
predictions <- predict(model, newdata = test_data)

# Calculate Root Mean Squared Error
rmse <- sqrt(mean((test_data$streams - predictions)^2))
cat("RMSE:", rmse, "\n")

# Calculate R-squared
sst <- sum((test_data$streams - mean(train_data$streams))^2)
ssr <- sum((predictions - mean(train_data$streams))^2)
r_squared <- ssr / sst
cat("R-squared:", r_squared, "\n")


# Best Performing Model: 
# The model with features related to platform presence and performance (Features 2) outperforms the others, 
# both in terms of lower RMSE and higher R-squared. This indicates that these platform-related features are 
# more predictive of streaming numbers compared to intrinsic musical qualities.

# Importance of Platform Presence: 
# The significant impact of being present in Spotify's playlists and charts across all models highlights the 
# importance of these platforms in influencing a song's streaming success.

# Model Fit and Predictive Power: 
# While the model with only intrinsic musical features has some predictive power, its overall fit is weak. The 
# combined model does not significantly outperform the platform-focused model, suggesting that the addition of 
# intrinsic features doesn’t add much predictive power beyond what is achieved by platform presence alone.