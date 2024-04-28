# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Author: Steve Kerr
# Date: April 2024
# ==============================================================================


# Setup ========================================================================
# Load packages
pacman::p_load(MASS, dplyr, here, readr, tidyr, install = FALSE)

# Load data 
pageviews_daily <- read_csv("3-data/wikipedia/pageviews-daily.csv")
pageviews_total <- read_csv(here("3-data/wikipedia/project-views-monthly.csv"))
cases_daily <- read_csv("3-data/mpox-cases/mpox-cases-daily.csv")
news_df <- read_csv(here("3-data/mpox-news/mpox-total-articles-deduplicated.csv"))
studies_df <- read_csv(here("3-data/mpox-studies/mpox-total-studies.csv"))
load(here("3-data/ref/iso_codes.RData"))

# Prepare data =================================================================
# Merge mpox cases and Wikipedia pageview data
mpox_df <- full_join(
  left_join(cases_daily, iso_ref, by = join_by(country == country_name, iso2, iso3)),
  left_join(pageviews_daily, iso_ref, by = join_by(iso2)), 
  by = join_by(country == country_name, iso2, iso3, date)
) |>
  select(country, iso2, iso3, project, wikidata_id, page_id, page_title, date, cases, pct_pageviews, pageviews, pageviews_ceil) |> 
  complete(fill = list(cases = 0))  |> # fill in missing cases with zeros
  arrange(country, date, page_title)

# Aggregate all mpox-specific articles
mpox_agg <- mpox_df |> 
  filter(
    page_title %in% c("Monkeypox", "Monkeypox virus", "Mpox"),
    iso3 == "USA",
    project == "en.wikipedia"
  ) |> 
  mutate(
    page_title = "Mpox", 
    page_id = 242702, # "Mpox" page ID
    wikidata_id = "Q382370", # "Mpox" Wikidata ID
  ) |> 
  reframe(
    .by = c(country, iso2, iso3, project, wikidata_id, page_id, page_title, date, cases),
    pageviews = sum(pageviews, na.rm = TRUE)
  ) |> 
  complete(page_title, date = seq.Date(min(pageviews_daily$date), to = max(pageviews_daily$date), by = 1), fill = list(cases = 0)) |>
  # Fill in missing info
  fill(country, iso2, iso3, project, wikidata_id, page_id, page_title, .direction = "updown") |> 
  # Normalize by dividing by total monthly country project views 
  mutate(year = year(date), month = month(date)) |> 
  left_join(pageviews_total, by = join_by(iso2, project, year, month), relationship = "many-to-one") |> 
  mutate(pct_pageviews = pageviews / pageviews_ceil) |> 
  select(country, iso2, iso3, project, wikidata_id, page_id, page_title, date, cases, pct_pageviews, pageviews, pageviews_ceil)

# Append aggregate mpox-specific figures with data
mpox_df <- mpox_df |> 
  filter(!page_title %in% c("Monkeypox", "Monkeypox virus", "Mpox")) |> 
  bind_rows(mpox_agg) |> 
  arrange(date) 


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


# Implement inclusion criteria =================================================
mpox_df <- mpox_df |> filter(iso3 == "USA", project == "en.wikipedia")


# Save data ====================================================================
write_csv(mpox_df, here("3-data/output/mpox-data.csv"))
