# GNews API 
api_key <- Sys.getenv("GNEWS_API_KEY")
search_query <- "monkeypox"
start_date <- as_date("2022-01-01")
end_date <- as_date("2022-05-01")
date_sequence <- seq.Date(start_date, end_date, by = 1)

news_df <- data.frame()

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

print(news_df)

# save results
write_csv(news_df, here("3-data/mpox-news/mpox-total-articles.csv"))

news_df <- read_csv(here("3-data/mpox-news/mpox-total-articles.csv"))

# plot daily number of articles
news_df |> 
  ggplot(aes(x = date, y = n_articles)) + 
  geom_col() + 
  theme_minimal()

# plot weekly number of articles
news_df |> 
  mutate(date = ceiling_date(date, unit = "weeks", week_start = 1)) |> 
  reframe(
    .by = c(search_term, date),
    n_articles = sum(n_articles, na.rm = TRUE)
  ) |> 
  ggplot(aes(x = date, y = n_articles)) +
  geom_col() + 
  theme_minimal()
