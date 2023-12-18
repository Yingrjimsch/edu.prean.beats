library(ggplot2)

spotify_data <- read.csv("./edu.prean.beats/Datenbereinigung/spotify-2023.csv", stringsAsFactors = FALSE)

spotify_data$streams <- as.numeric(gsub("[^0-9]", "", spotify_data$streams))

spotify_data <- na.omit(spotify_data)

cat("Minimum streams:", min(spotify_data$streams), "\n")
cat("Maximum streams:", max(spotify_data$streams), "\n")
cat("Quantiles of streams:", quantile(spotify_data$streams, probs = c(0.25, 0.5, 0.75)), "\n")

quantile_99 <- quantile(spotify_data$streams, 0.99)
spotify_data <- spotify_data[spotify_data$streams <= quantile_99, ]

spotify_data$streams_log <- log1p(spotify_data$streams)

p <- ggplot(spotify_data, aes(x = streams_log)) +
    geom_density(fill = "steelblue", alpha = 0.7) +
    xlim(0, 30) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12)
    ) +
    labs(
      title = "Log-transformed Density Plot of Spotify Streams",
      x = "Log(Number of Streams)",
      y = "Density"
    )

ggsave("./edu.prean.beats/visualization/plots/density_plot_streams_log.png", plot = p, width = 10, height = 6, dpi = 300)

# Log Transformation
# number of streams = e**(log-transformed-value) - 1
# e.g. number of streams at x = 20 => e**20 - 1 = ~ 485 Mio.
