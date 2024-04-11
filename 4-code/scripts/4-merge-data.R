# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================


# Setup ========================================================================
# load Wikipedia pageview data
pageviews_weekly <- read_csv("3-data/wikipedia/pageviews-weekly.csv")

# load mpox case data 
cases_weekly <- read_csv("3-data/mpox-cases/mpox-cases-weekly.csv")

# load ISO code reference table
load(here("3-data/ref/iso_codes.RData"))

# Prepare data =================================================================
# merge mpox cases and Wikipedia pageview data
mpox_df <- full_join(
  left_join(cases_weekly, iso_ref, by = join_by(iso3)),
  left_join(pageviews_weekly, iso_ref, by = join_by(iso2)), 
  by = join_by(country_name, iso2, iso3, date)
  ) |>
  select(country = country_name, iso2, iso3, cases, project, wikidata_id, page_title, page_id, date, pct_pageviews, pageviews, pageviews_ceil) |> 
  complete(fill = list(pageviews = 0, pct_pageviews = 0, cases = 0))  # fill in missing zero

# TODO: Should I drop NAs or impute missing values here?

# NOTE: China's page view is excluded from Differential Privacy version (source: https://foundation.wikimedia.org/wiki/Legal:Country_and_Territory_Protection_List)
# NOTE: Pageview data for the Democratic Republic of the Congo not available, presumably due to the low pageviews since its data should be published if it does exist...
# Can include these points in the discussion


# Implement inclusion criteria =================================================
mpox_df <- mpox_df |> filter(iso3 == "USA", project == "en.wikipedia")


# Save data ====================================================================
write_csv(mpox_df, here("3-data/output/mpox-data.csv"))
