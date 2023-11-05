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
