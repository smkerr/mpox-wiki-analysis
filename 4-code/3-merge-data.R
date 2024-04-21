# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================


# Setup ========================================================================
# load Wikipedia pageview data
pageviews_daily <- read_csv("3-data/wikipedia/pageviews-daily.csv")

# load mpox case data 
cases_daily <- read_csv("3-data/mpox-cases/mpox-cases-daily.csv")

# load mpox news coverage data 
news_df <- read_csv(here("3-data/mpox-news/mpox-total-articles-deduplicated.csv"))

# load mpox studies data
studies_df <- read_csv(here("3-data/mpox-studies/mpox-total-studies.csv"))

# load ISO code reference table
load(here("3-data/ref/iso_codes.RData"))

# Prepare data =================================================================
# merge mpox cases and Wikipedia pageview data
mpox_df <- full_join(
  left_join(cases_daily, iso_ref, by = join_by(country == country_name, iso2, iso3)),
  left_join(pageviews_daily, iso_ref, by = join_by(iso2)), 
  by = join_by(country == country_name, iso2, iso3, date)
  ) |>
  select(country, iso2, iso3, cases, project, wikidata_id, page_title, page_id, date, pct_pageviews, pageviews, pageviews_ceil) |> 
  filter(if_all(c(project:page_id, pct_pageviews:pageviews_ceil), ~ !is.na(.))) |> # drop missing observations
  complete(fill = list(cases = 0))  |> # fill in missing cases with zeros
  arrange(country, date, page_title)

# merge with mpox news data
mpox_df <- left_join(
  mpox_df,
  news_df |> 
    reframe(.by = date, n_articles = sum(n_articles)) |> # combine all article counts
    mutate(country = "United States"),
  by = join_by(country, date)
  )

# merge with mpox studies data
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
