library(ggplot2)

# Load the dataset using base R
spotify_data <- read.csv("../../spotify-2023.csv", stringsAsFactors = FALSE)

# Define a vibrant color palette
color_palette <- scale_fill_manual(values=c(rgb(0.2, 0.5, 0.8, 0.7)))

# Extract names of all numerical columns
numerical_cols <- names(spotify_data)[sapply(spotify_data, is.numeric)]

# Function to plot and save histogram for a given column
plot_and_save_histogram <- function(column_name, data) {
  p <- ggplot(data, aes(x=.data[[column_name]])) +
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
      title=paste("Distribution of", column_name),
      x=column_name,
      y="Density",
      fill="Count"
    )
  
  # Save the plot to a PNG file
  ggsave(paste0("./plots/histogram_", column_name, ".png"), plot = p, width = 10, height = 6, dpi = 300)
}

# Iterate through each numerical column and create the plots
for (col in numerical_cols) {
  plot_and_save_histogram(col, spotify_data)
}


