# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Author: Steve Kerr
# Date: April 2024
# ==============================================================================


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


# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================


# Data quality checks ----------------------------------------------------------
# mpox data 
agg_probable <- agg_total |> 
  filter(total_probable_cases > 0) |> 
  group_by(country) |> 
  mutate(
    total_cases = sum(total_confirmed_cases, total_probable_cases),
    pct_probable_cases = total_probable_cases / total_cases * 100) |> 
  arrange(desc(pct_probable_cases)) 

agg_probable |> 
  ggplot(aes(x = reorder(country, pct_probable_cases), y = pct_probable_cases)) + 
  geom_col() + 
  labs(
    title = "Percentage of probable cases, by country",
    subtitle = "data as of 31 Dec 2023 17:00 CET",
    x = "",
    y = "Percentage of probable cases"
  ) + 
  coord_flip() + 
  theme_minimal()

# Wikipedia data
# frequency of projects and page names
pageviews[1:2] |> 
  map(~ tabyl(.x)) |> 
  set_names(names(pageviews[1:2]))

# % missing by column
pageviews |> 
  map_df(~sum(is.na(.)/length(.)) * 100)

# calculate max redirects
max_redirects <- pageviews |> 
  tabyl(redirect_name, page_name, show_na = FALSE) |> 
  select(where(is.numeric)) |> 
  max()

# redirects heat map
pageviews_tbl <- pageviews |> 
  tabyl(redirect_name, page_name, show_na = FALSE) |> 
  gt() |> 
  cols_label(
    redirect_name = "Redirected search"
  ) |> 
  tab_header(
    title = "Redirects for user views",
    subtitle = "English Wikipedia"
  ) |> 
  tab_spanner(
    label = "Articles",
    columns  = !redirect_name
  ) |>
  data_color(
    columns = !redirect_name,
    colors = col_numeric(
      palette = c("white", "red"),
      domain = c(0, max_redirects))
  ) 
pageviews_tbl


# Check differentially private pageviews ---------------------------------------
df |> 
  tabyl(country, project) |> 
  group_by(country)
slice_max()

labels <- df |> 
  tabyl(project, country) |>
  select(project)
totals <- df |> 
  tabyl(project, country) |> 
  select(-project) |> 
  rowSums()
combo <- cbind(labels, totals) |> 
  arrange(totals)
combo |> 
  mutate(project = factor(combo$project, levels = combo$project)) |> 
  filter(totals >= 15) |> 
  ggplot(aes(x = project, y = totals)) + 
  geom_col() + 
  labs(
    title = "Number of overall differentially private pageviews",
    subtitle = "for select countries on 15 June 2022",
    y = "Number of pageviews (differentially private)",
    x = NULL
  ) +
  theme_minimal() + 
  coord_flip()

pie(totals, labels = labels, man = "Share of differentially private pageviews by language")

combo |> 
  slice_max(totals, n = 10) |> 
  mutate(project = factor(project, levels = slice_max(combo, totals, n = 10)$project)) |> 
  ggplot(aes(x = "", y = totals, fill = project)) + 
  geom_col(width = 1, color = "white") + 
  coord_polar(theta = "y") + 
  labs(
    title = "Share of pageviews (differentially private) by language",
    subtitle = "Top 10 most common languages in selected countries",
    fill = "Wikipedia project"
  ) +
  theme_void() 
