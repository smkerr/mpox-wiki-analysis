# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================


# Get number of total mpox-related news articles ===============================
api_key <- Sys.getenv("GNEWS_API_KEY")
search_terms <- c("monkeypox", "mpox")
start_date <- as_date("2022-01-01") 
end_date <- as_date("2023-02-05")
date_sequence <- seq.Date(start_date, end_date, by = 1)

if (file.exists(here("3-data/mpox-news/mpox-total-articles.csv"))) {
  news_df <- read_csv(here("3-data/mpox-news/mpox-total-articles.csv"))  
} else news_df <- data.frame()

for (search_query in search_terms) {
  for (date in date_sequence) {
    # Construct API URL
    url <- glue("https://gnews.io/api/v4/search?q={URLencode(search_query)}&lang=en&country=us&max=10&from={as_date(date)}T00:00:00Z&to={as_date(date)}T23:59:59Z&apikey={api_key}")
    
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


# Get headlines of mpox-related news articles ==================================
# Time period where "monkeypox" and "mpox" were both in use
start_date <- as_date("2023-01-09") ### TODO: still need to query 28 Nov 2022
end_date <- as_date("2023-02-05") ####
date_sequence <- seq.Date(start_date, end_date, by = 1)

if (file.exists(here("3-data/mpox-news/mpox-article-headlines.csv"))) {
  headlines_df <- read_csv(here("3-data/mpox-news/mpox-article-headlines.csv"))  
} else headlines_df <- data.frame()


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

# save results
write_csv(headlines_df, here("3-data/mpox-news/mpox-article-headlines.csv"))

# Create de-duplicated news article count dataframe ============================
news_df <- bind_rows(
  news_df |> 
    filter(date < as_date("2022-11-28")) |> 
    select(-search_term),
  headlines_df |> 
    mutate(date = as_date(publishedAt)) |> 
    distinct(date, title, description, content) |> # de-duplicate 
    count(date, name = "n_articles")
  ) |> 
  arrange(date)

# save results
write_csv(news_df, here("3-data/mpox-news/mpox-total-articles-deduplicated.csv"))
  