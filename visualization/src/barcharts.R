library(ggplot2)

spotify_data <- read.csv("../../spotify-2023.csv", 
                         fileEncoding = "ISO-8859-1", stringsAsFactors = FALSE)

# Clean the column names by removing the trailing "_." if present
names(spotify_data) <- sub("_\\.$", "", names(spotify_data))

# Assuming streams is already numeric, if not, clean it
spotify_data$streams <- as.numeric(gsub("[^0-9.-]", "", spotify_data$streams))

# Remove rows with NA in the key, mode, or streams column
spotify_data <- na.omit(spotify_data[c("key", "mode", "streams")])


plot_and_save_bar_chart <- function(column_name, data) {

  # Compute mean streams for each category
  category_means <- aggregate(streams ~ get(column_name), data, mean)
  names(category_means) <- c(column_name, "MeanStreams")

  p <- ggplot(category_means, aes_string(x = column_name, y = 'MeanStreams', fill = column_name)) +
    geom_bar(stat = "identity", color = "black", alpha = 0.7) +
    scale_fill_viridis_d() +
    theme_minimal() +
    theme(
      panel.grid.major = element_line(linetype = "dotted", color = "grey85"),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12)
    ) +
    labs(
      title = paste("Mean Streams by", column_name),
      x = column_name,
      y = "Mean Number of Streams"
    )
  
  ggsave(paste0("../plots/bar_chart_", column_name, ".png"), plot = p, width = 10, height = 6, dpi = 300)
}

# Apply function to 'key' and 'mode' columns
plot_and_save_bar_chart('key', spotify_data)
plot_and_save_bar_chart('mode', spotify_data)
