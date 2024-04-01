# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================


# Prepare data =================================================================
# load ISO code reference table
load(here("3-data/ref/iso_codes.RData"))

# merge ISO codes 
cases_iso <- left_join(cases_df, iso_ref, by = join_by(iso3))
pageviews_iso <- left_join(pageviews_mpox, iso_ref, by = join_by(iso2))

# merge mpox cases and Wikipedia pageview data
mpox_df <- full_join(cases_iso, pageviews_iso, by = join_by(country_name, iso2, iso3, date)) |>
  select(country = country_name, iso2, iso3, date, pageviews_est, cases, cases_moving_avg) |> 
  complete(fill = list(cases = 0, cases_moving_avg = 0)) |>  # fill in missing zeros
  filter(!is.na(pageviews_est)) # exclude insufficient pageview data

### test 
#mpox_df <- mpox_df |> 
#  filter(iso3 == "USA")
