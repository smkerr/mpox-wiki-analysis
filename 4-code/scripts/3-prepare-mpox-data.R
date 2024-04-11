# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================


# Load data ====================================================================
# load mpox case data
cases_df <- read_csv("https://raw.githubusercontent.com/owid/monkeypox/main/owid-monkeypox-data.csv")
write_csv(cases_df, here(glue("3-data/mpox-cases/owid-monkeypox-data ({format(today(), '%Y-%m-%d')}).csv")))


# Prepare data =================================================================
# daily region-level case data
cases_region_df <- cases_df |> 
  select(region = location, iso3 = iso_code, date, cases = new_cases, cases_moving_avg = new_cases_smoothed) |> 
  filter(str_detect(iso3, "OWID"), region != "World") |> # only keep region-level counts
  mutate(date = ceiling_date(date, unit = "weeks", week_start = 3)) |> # calculate weekly cases
  group_by(region, iso3, date) |> 
  summarize(cases = sum(cases, na.rm = TRUE)) |> 
  ungroup()

# order regions by total cases
cases_region_ordered <- cases_region_df |> 
  reframe(cases = sum(cases), .by = c(region)) |> 
  arrange(-cases)
cases_region_df <- cases_region_df |> 
  mutate(region = factor(region, levels = cases_region_ordered$region))

# daily country-level case data
cases_daily <- cases_df |> 
  select(country = location, iso3 = iso_code, date, cases = new_cases, cases_moving_avg = new_cases_smoothed) |> 
  filter(!str_detect(iso3, "OWID")) # remove region-level counts

# weekly country-level case data 
cases_weekly <- cases_daily |> 
  mutate(date = ceiling_date(date, unit = "weeks", week_start = 1)) |> ###
  group_by(country, iso3, date) |> 
  summarize(cases = sum(cases, na.rm = TRUE)) |> 
  ungroup()


# TODO: Will need to either aggregate or disaggregate China's cases and pageviews
# Currently cases are aggregated while pageviews are disaggregated
# As of 20 March 2024: Cases shown include those in mainland China (1611), Hong Kong SAR (83), Taipei (335), and Macao (2)


# Save data ====================================================================
write_csv(cases_daily, here("3-data/mpox-cases/mpox-cases-daily.csv"))
write_csv(cases_weekly, here("3-data/mpox-cases/mpox-cases-weekly.csv"))
