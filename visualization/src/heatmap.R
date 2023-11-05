library(ggplot2)
library(reshape2)

spotify_data <- read.csv("../../spotify-2023.csv", fileEncoding = "ISO-8859-1", stringsAsFactors = FALSE)

# Clean the column names by removing the trailing "_." if present
names(spotify_data) <- sub("_\\.$", "", names(spotify_data))

# Get the names of all numerical columns that do not have missing values
numerical_columns <- names(spotify_data)[sapply(spotify_data, is.numeric) & !colSums(is.na(spotify_data))]

# Correlation matrix
corr_matrix <- cor(spotify_data[numerical_columns], use = "complete.obs")

# Melt correlation matrix into format suitable for ggplot2
melted_corr_matrix <- melt(corr_matrix)

# Plot heatmap
p <- ggplot(data = melted_corr_matrix, aes(Var1, Var2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
          axis.text.y = element_text(size = 12)) +
    labs(x = '', y = '', title = 'Correlation Matrix Heatmap') 

# Save plot
ggsave("../plots/heatmap_correlation_matrix.png", plot = p, width = 12, height = 10, dpi = 300)

# Heatmap correlation results
# Positive correlations:
#   *in_spotify_playlists and in_apple_playlists: This suggests that songs which are in Spotify playlists 
#   are also likely to be in Apple playlists. This might indicate that popular songs tend to be featured across different streaming platforms.
#   *in_deezer_charts and in_apple_charts: Similarly, this suggests that songs charting on Deezer are also likely to chart on Apple Music.
#   in_spotify_charts and in_apple_charts: There is a strong correlation indicating that songs which are on Spotify charts tend also to be on Apple charts, 
#   which again could reflect a general trend in music popularity across platforms.
#   *bpm (beats per minute) and energy: This is a common correlation in music where higher BPM tends to be associated with more energetic tracks.
#   *danceability and valence: Danceable songs often have a higher valence, meaning they tend to be more joyful or positive.
# Negative correlations:
#   There are not many strong negative correlations in this heatmap, but some mild negative correlations between artist_count and variables 
#   such as in_spotify_charts, in_deezer_charts, in_apple_charts. This could suggest that songs by solo artists or fewer artists might be more prevalent in 
#   charts than songs by multiple artists, though the correlation is not very strong.