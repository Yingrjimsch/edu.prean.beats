library(ggplot2)

spotify_data <- read.csv("../../spotify-2023.csv", fileEncoding = "ISO-8859-1", stringsAsFactors = FALSE)

# Clean up the column names if necessary
names(spotify_data) <- sub("_\\.$", "", names(spotify_data))

# Check if 'released_year', 'released_month', 'released_day' columns are numeric, convert if not
spotify_data$released_year <- as.numeric(as.character(spotify_data$released_year))
spotify_data$released_month <- as.numeric(as.character(spotify_data$released_month))
spotify_data$released_day <- as.numeric(as.character(spotify_data$released_day))
spotify_data$streams <- as.numeric(gsub("[^0-9.-]", "", as.character(spotify_data$streams)))

# Remove rows with NA in 'released_year', 'released_month', 'released_day', or 'streams'
spotify_data <- na.omit(spotify_data[c("released_year", "released_month", "released_day", "streams")])

# Print out how many rows have dates before the year 2000
num_songs_before_2000 <- sum(spotify_data$released_year < 2000)
cat("Number of songs with release years before 2000:", num_songs_before_2000, "\n")

# Check for validity of the release date
valid_date_idx <- with(spotify_data, released_year >= 2000 & released_month >= 1 & released_month <= 12 & released_day >= 1 & released_day <= 31)

# Subset the data to only include valid dates
spotify_data <- spotify_data[valid_date_idx, ]

# Create a Date column from the year, month, and day columns
spotify_data$Date <- as.Date(with(spotify_data, paste(released_year, released_month, released_day, sep="-")), "%Y-%m-%d")

# Aggregate total streams by Date
daily_streams <- aggregate(streams ~ Date, spotify_data, sum)

p <- ggplot(daily_streams, aes(x = Date, y = streams)) +
    geom_line(color = "blue", size = 1) +
    theme_minimal() +
    theme(
      panel.grid.major = element_line(linetype = "dotted", color = "grey85"),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12)
    ) +
    labs(
      title = "Total Number of Streams Over Time",
      x = "Release Date",
      y = "Total Streams"
    )

ggsave("../plots/time_series_streams.png", plot = p, width = 12, height = 6, dpi = 300)
