# data_process.R

# Load necessary libraries
library(readr)
library(dplyr)
library(lubridate)
library(maps)

# Function to load and clean data
load_and_clean_data <- function() {
  # Load the data
  movie_df <- readr::read_csv("data/movies.csv")

  # Calculate profitability (gross - budget) for each movie
  movie_df$profit <- movie_df$gross - movie_df$budget

  # Create columns containing the release data, month, season, and decade
  movie_df$date <- as.Date(movie_df$released, format = "%B %d, %Y")
  movie_df$month <- lubridate::month(movie_df$date)
  movie_df$season <- cut(
    movie_df$month %% 12 + 1,
    breaks = c(0, 3, 6, 9, 12),
    labels = c("Winter", "Spring", "Summer", "Fall"),
    include.lowest = TRUE
  )

  get_decade <- function(year) {
    if (year >= 1980 && year < 1990) {
      return("1980s")
    } else if (year >= 1990 && year < 2000) {
      return("1990s")
    } else if (year >= 2000 && year < 2010) {
      return("2000s")
    } else if (year >= 2010 && year < 2020) {
      return("2010s")
    } else if (year >= 2020) {
      return("2020s")
    } else {
      return("Unknown")
    }
  }

  movie_df <- movie_df %>%
    mutate(decade = sapply(year, get_decade))

  # Capitalize the names of the "budget" and "profit" columns
  colnames(movie_df)[colnames(movie_df) %in% c("budget", "profit")] <- c("Budget", "Profit")

  movie_df <- movie_df %>%
    filter(
      !is.na(genre),
      !is.na(Budget),
      !is.na(Profit),
      !is.na(runtime),
      !is.na(rating),
      !is.na(score),
      !is.na(decade),
      !is.na(season),
      year %in% 1980:2019
    )

    movie_df$country <- ifelse(movie_df$country == "United Kingdom", "UK", movie_df$country)
    movie_df$country <- ifelse(movie_df$country == "United States", "USA", movie_df$country)
    movie_df$country <- ifelse(movie_df$country == "West Germany", "Germany", movie_df$country)
    movie_df$country <- ifelse(movie_df$country == "Hong Kong", "China", movie_df$country)
    movie_df$country <- ifelse(movie_df$country == "Yugoslavia", "Serbia", movie_df$country)

  return(movie_df)
}
