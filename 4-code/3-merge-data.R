# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================


# Setup ========================================================================
# Load packages
pacman::p_load(dplyr, here, readr, tidyr)
# Load Wikipedia pageview data
pageviews_daily <- read_csv("3-data/wikipedia/pageviews-daily.csv")

# Load mpox case data 
cases_daily <- read_csv("3-data/mpox-cases/mpox-cases-daily.csv")

# Load mpox news coverage data 
news_df <- read_csv(here("3-data/mpox-news/mpox-total-articles-deduplicated.csv"))

# Load mpox studies data
studies_df <- read_csv(here("3-data/mpox-studies/mpox-total-studies.csv"))

# Load ISO code reference table
load(here("3-data/ref/iso_codes.RData"))

# Prepare data =================================================================
# Merge mpox cases and Wikipedia pageview data
mpox_df <- full_join(
  left_join(cases_daily, iso_ref, by = join_by(country == country_name, iso2, iso3)),
  left_join(pageviews_daily, iso_ref, by = join_by(iso2)), 
  by = join_by(country == country_name, iso2, iso3, date)
  ) |>
  select(country, iso2, iso3, project, wikidata_id, page_id, page_title, date, cases, pct_pageviews, pageviews, pageviews_ceil) |> 
  #filter(if_all(c(project:page_id, pct_pageviews:pageviews_ceil), ~ !is.na(.))) |> # drop missing observations
  complete(fill = list(cases = 0))  |> # fill in missing cases with zeros
  arrange(country, date, page_title)

# Merge with mpox news data
mpox_df <- left_join(
  mpox_df,
  news_df |> 
    reframe(.by = date, n_articles = sum(n_articles)) |> # combine all article counts
    mutate(country = "United States"),
  by = join_by(country, date)
  ) |> 
  complete(fill = list(n_articles = 0))

# Merge with mpox studies data
mpox_df <- left_join(
  mpox_df,
  studies_df |> 
    reframe(.by = date, n_studies = n()) |> 
    arrange(date) ,
  by = join_by(date)
  ) |> 
  complete(fill = list(n_studies = 0))

# Aggregate all mpox-specific articles
mpox_agg <- mpox_df |> 
  filter(page_title %in% c("Monkeypox", "Monkeypox virus", "Mpox")) |> 
  reframe(
    .by = c(country, iso2, iso3, project, date, cases, n_articles, n_studies),
    pageviews = sum(pageviews, na.rm = TRUE),
    pageviews = ifelse(pageviews == 0, NA_integer_, pageviews),
    pageviews_ceil,
    pct_pageviews = sum(pageviews, na.rm = TRUE) / pageviews_ceil,
    page_title = "Mpox", 
    page_id = 242702, # "Mpox" page ID
    wikidata_id = "Q382370" # "Mpox" Wikidata ID 
  ) |>
  #filter(pageviews != 0) |> 
  distinct() |> # remove duplicates
  arrange(date) |> 
  select(country, iso2, iso3, project, wikidata_id, page_id, page_title, date, cases, pct_pageviews, pageviews, pageviews_ceil, n_articles, n_studies )

# Append aggregate mpox-specific figures with data
mpox_df <- mpox_df |> 
  filter(!page_title %in% c("Monkeypox", "Monkeypox virus", "Mpox")) |> 
  bind_rows(mpox_agg) |> 
  arrange(date) 


# Implement inclusion criteria =================================================
mpox_df <- mpox_df |> filter(iso3 == "USA", project == "en.wikipedia")

# Save data ====================================================================
write_csv(mpox_df, here("3-data/output/mpox-data.csv"))
