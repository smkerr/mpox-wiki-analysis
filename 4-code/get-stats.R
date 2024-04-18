# load pageviews data
pageviews_df <- read_csv(here("3-data/wikipedia/pageviews-daily.csv"))
pageviews_daily <- read_csv(here("3-data/wikipedia/pageviews-daily.csv"))
pageviews_weekly <- read_csv(here("3-data/wikipedia/pageviews-weekly.csv"))
pageviews_total <- read_csv(here("3-data/wikipedia/total-pageviews.csv"))

# load mpox case data
cases_daily <- read_csv(here("3-data/mpox-cases/mpox-cases-daily.csv"))
cases_weekly <- read_csv(here("3-data/mpox-cases/mpox-cases-weekly.csv"))
cases_total <- read_csv(here("3-data/mpox-cases/mpox-cases-total.csv"))

# load media coverage data 
news_df <- read_csv(here("3-data/mpox-news/mpox-total-articles.csv"))


# Pageviews ====================================================================



# Mpox cases ===================================================================
# date range of mpox cases 
range(cases_daily$date)

# peak in daily cases
cases_daily |> 
  arrange(-cases) |> 
  slice_head()

# peak in weekly cases
cases_weekly |> 
  arrange(-cases) |> 
  slice_head()

# Media coverage ===============================================================
# TODO: combine "monkeypox" and 

# plot daily number of articles
news_df |> 
  reframe(.by = date, n_articles = sum(n_articles)) |> # combine "monkeypox" and "mpox" articles
  ggplot(aes(x = date, y = n_articles)) + 
  geom_col() + 
  theme_minimal()

# plot daily number of articles by search term
news_df |> 
  ggplot(aes(x = date, y = n_articles)) +
  geom_col() + 
  facet_wrap(~search_term, ncol = 1) +
  theme_minimal()

# plot weekly number of articles
news_df |> 
  mutate(date = floor_date(date, unit = "weeks")) |> 
  reframe(.by = date, n_articles = sum(n_articles)) |> # combine "monkeypox" and "mpox" articles
  ggplot(aes(x = date, y = n_articles)) +
  geom_col() + 
  theme_minimal() 

# plot weekly number of articles by search term
news_df |> 
  mutate(date = floor_date(date, unit = "weeks")) |> 
  reframe(.by = c(search_term, date), n_articles = sum(n_articles)) |> # combine "monkeypox" and "mpox" articles
  ggplot(aes(x = date, y = n_articles)) +
  geom_col() + 
  facet_wrap(~search_term, ncol = 1) +
  theme_minimal() 
