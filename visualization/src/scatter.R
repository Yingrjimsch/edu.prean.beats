library(ggplot2)

spotify_data <- read.csv("../../spotify-2023.csv", stringsAsFactors = FALSE)

spotify_data$artist_count <- as.numeric(spotify_data$artist_count)
spotify_data$in_spotify_charts <- as.numeric(spotify_data$in_spotify_charts)
spotify_data$in_deezer_charts <- as.numeric(spotify_data$in_deezer_charts)
spotify_data$in_apple_charts <- as.numeric(spotify_data$in_apple_charts)

spotify_data <- na.omit(spotify_data[, c("artist_count", "in_spotify_charts", "in_deezer_charts", "in_apple_charts")])

plot_correlation <- function(data, x_var, y_var) {

  corr_coef <- cor(data[[x_var]], data[[y_var]], method = "pearson")
  cat(paste("Correlation between", x_var, "and", y_var, ":", corr_coef, "\n"))

  p <- ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", color = "red") +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12)
    ) +
    labs(
      title = paste("Scatter Plot of", x_var, "vs", y_var),
      x = x_var,
      y = y_var
    )
  
  ggsave(paste("../plots/scatter_", x_var, "_vs_", y_var, ".png"), plot = p, width = 10, height = 6, dpi = 300)
}

plot_correlation(spotify_data, "artist_count", "in_spotify_charts")
plot_correlation(spotify_data, "artist_count", "in_deezer_charts")
plot_correlation(spotify_data, "artist_count", "in_apple_charts")
