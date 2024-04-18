# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================

# TODO: Generalize so that it loops through mpox and monkeypox search terms


# GNews API 
api_key <- Sys.getenv("GNEWS_API_KEY")
search_query <- "mpox"
start_date <- as_date("2022-11-27") # date WHO announced name change to mpox
end_date <- as_date("2023-02-05")
date_sequence <- seq.Date(start_date, end_date, by = 1)

if (file.exists(here("3-data/mpox-news/mpox-total-articles.csv"))) {
  news_df <- read_csv(here("3-data/mpox-news/mpox-total-articles.csv"))  
} else news_df <- data.frame()


for (date in date_sequence) {
  
  url <- glue("https://gnews.io/api/v4/search?q={URLencode(search_query)}&lang=en&country=us&max=10&from={as_date(date)}T00:00:00Z&to={as_date(date)}T23:59:59Z&apikey={api_key}")
  
  response <- GET(url)
  
  parsed_content <- content(response, as = "parsed")
  
  total_articles_df <- tibble(
    search_term = search_query,
    date = as_date(date),
    n_articles = parsed_content$totalArticles 
  )
  news_df <- bind_rows(news_df, total_articles_df)
  
  Sys.sleep(1)
}

# save results
write_csv(news_df, here("3-data/mpox-news/mpox-total-articles.csv"))
