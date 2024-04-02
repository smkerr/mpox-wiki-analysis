# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================


# Prepare data =================================================================
# load ISO code reference table
load(here("3-data/ref/iso_codes.RData"))

# merge ISO codes 
cases_iso <- left_join(cases_df, iso_ref, by = join_by(country == country_name, iso3))
pageviews_iso <- left_join(pageviews_df, iso_ref, by = join_by(country == country_name, iso2, iso3))

# merge mpox cases and Wikipedia pageview data
mpox_df <- full_join(cases_iso, pageviews_iso, by = join_by(country, iso2, iso3, date)) |>
  select(country, iso2, iso3, date, est_pageviews, est_pct_pageviews, cases, cases_moving_avg) |> 
  complete(fill = list(est_pageviews = 0, est_pct_pageviews = 0, cases = 0, cases_moving_avg = 0))  # fill in missing zero

### test 
#mpox_df <- mpox_df |> 
#  filter(iso3 == "USA")
