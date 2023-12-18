library(ggplot2)

spotify_data <- read.csv("../../spotify-2023.csv", stringsAsFactors = FALSE)

# Color palette
color_palette <- scale_fill_manual(values=c(rgb(0.2, 0.5, 0.8, 0.7)))

# Extract names of all numerical columns
numerical_cols <- names(spotify_data)[sapply(spotify_data, is.numeric)]

plot_and_save_histogram <- function(column_name, data) {
  clean_column_name <- sub("_\\.$", "", column_name)
  p <- ggplot(data, aes(x=.data[[clean_column_name]])) +
    geom_histogram(aes(y=after_stat(density), fill=after_stat(count)), bins=20, color="grey20", alpha=0.8) +
    scale_fill_gradient(low="lightblue", high="steelblue") +
    theme_minimal() +
    theme(
      panel.grid.major = element_line(linetype = "dotted", color = "grey85"),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size=14, face="bold"),
      axis.title = element_text(size=12)
    ) +
    labs(
      title=paste("Distribution of", clean_column_name),
      x=clean_column_name,
      y="Density",
      fill="Count"
    )
  
  # Save plot to PNG file
  ggsave(paste0("../plots/histogram_", clean_column_name, ".png"), plot = p, width = 10, height = 6, dpi = 300)
}

# Iterate through each numerical column and create plots
for (col in numerical_cols) {
  plot_and_save_histogram(col, spotify_data)
}


