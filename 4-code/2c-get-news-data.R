# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Author: Steve Kerr
# Date: April 2024
# ==============================================================================


# Setup ========================================================================
# Load packages
pacman::p_load(
  MASS,
  dplyr,
  glue,
  here,
  httr,
  lubridate,
  purrr,
  readr,
  tibble,
  tidyr,
  install = FALSE
  )

# Set API key
api_key <- Sys.getenv("GNEWS_API_KEY")

# Define search terms
search_terms <- c("monkeypox", "mpox")

# Get number of total mpox-related news articles ===============================
if (file.exists(here("3-data/mpox-news/mpox-total-articles.csv"))) {
  news_df <- read_csv(here("3-data/mpox-news/mpox-total-articles.csv"))  
} else {
  # Initialize empty df to store data
  news_df <- data.frame()

  # Key parameters
  start_date <- as_date("2022-01-01") 
  end_date <- as_date("2022-11-27")
  date_sequence <- seq.Date(start_date, end_date, by = 1)
  
  for (search_query in search_terms) {
    for (date in date_sequence) {
      # Construct API URL
      url <- glue("https://gnews.io/api/v4/search?q={URLencode(search_query)}&lang=en&country=us&max=10&from={as_date(date  )}T00:00:00Z&to={as_date(date)}T23:59:59Z&apikey={api_key}")
      
      # Perform the GET request
      response <- GET(url)
      if (status_code(response) == 200) {
        parsed_content <- content(response, as = "parsed")
        
        # Create a data frame for the current date's articles
        total_articles_df <- tibble(
          search_term = search_query,
          date = as_date(date),
          n_articles = parsed_content$totalArticles 
        )
        
        # Append to the main data frame
        news_df <- bind_rows(news_df, total_articles_df)
      }
      
      # Respect API rate limits
      Sys.sleep(1)
    }
  }
  # save results
  write_csv(news_df, here("3-data/mpox-news/mpox-total-articles.csv"))
}


# Get headlines of mpox-related news articles ==================================
if (file.exists(here("3-data/mpox-news/mpox-article-headlines.csv"))) {
  headlines_df <- read_csv(here("3-data/mpox-news/mpox-article-headlines.csv"))  
} else {
  # Initialize empty df to store data
  headlines_df <- data.frame()

  # Key parameters
  start_date <- as_date("2022-11-28")
  end_date <- as_date("2024-02-27")
  date_sequence <- seq.Date(start_date, end_date, by = 1)

  for (search_query in search_terms) {
    for (date in date_sequence) {
      # Construct API URL
      url <- glue("https://gnews.io/api/v4/search?q={URLencode(search_query)}&lang=en&country=us&max=10&from={as_date(date)}T00:00:00Z&to={as_date(date)}T23:59:59Z&apikey={api_key}")
    
      # Perform the GET request
      response <- GET(url)
      if (status_code(response) == 200) {
        parsed_content <- content(response, as = "parsed")
        
        # Create a data frame for the current date's articles
        if (parsed_content$totalArticles > 0) {
          articles_df <- parsed_content |> 
            pluck("articles") |> 
            map_df(unlist) |> 
            mutate(search_term = search_query, publishedAt = ymd_hms(publishedAt))
          
          # Append to the main data frame
          headlines_df <- bind_rows(headlines_df, articles_df)
        }
      }
      
      # Respect API rate limits
      Sys.sleep(1)
    }
  }
}


# Create de-duplicated news article count dataframe ============================
# Full date range
date_sequence <- seq.Date(from = as_date("2022-01-01"), to = as_date("2024-02-27"), by = 1) |> 
  as_tibble_col(column_name = "date")

news_deduplicated <- bind_rows(
  news_df |> # news article counts before name change
    filter(date < as_date("2022-11-28")) |> 
    select(-search_term),
  headlines_df |> # news article counts after name change
    mutate(date = as_date(publishedAt)) |> 
    distinct(date, title, description, content) |> # de-duplicate 
    count(date, name = "n_articles")
  ) |> 
  right_join(date_sequence, by = join_by(date)) |> 
  replace_na(list(n_articles = 0)) |> # impute zeroes for date without articles
  arrange(date)

# save results
write_csv(news_deduplicated, here("3-data/mpox-news/mpox-total-articles-deduplicated.csv"))
  