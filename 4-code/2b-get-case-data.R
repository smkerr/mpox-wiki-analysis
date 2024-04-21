# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================


# Load data ====================================================================
# load WHO/OWID mpox case data
cases_df <- read_csv("https://raw.githubusercontent.com/owid/monkeypox/main/owid-monkeypox-data.csv")
write_csv(cases_df, here(glue("3-data/mpox-cases/owid-monkeypox-data ({format(today(), '%Y-%m-%d')}).csv")))

# load CDC mpox case data
cases_daily <- read_csv(here("3-data/mpox-cases/cdc-mpox-cases-daily.csv")) |> 
  select(date = Epi_date_V3, cases = Cases, moving_avg_cases = `7-Day Average`) |> 
  mutate(country = "United States", iso2 = "US", iso3 = "USA", date = mdy(date)) |> 
  relocate(c(country, iso2, iso3), .before = everything())

# Prepare data =================================================================
# Global data 
## daily country-level case data
cases_country_df <- cases_df |> 
  select(country = location, iso3 = iso_code, date, cases = new_cases, cases_moving_avg = new_cases_smoothed) |> 
  filter(!str_detect(iso3, "OWID"), country != "World") |> # only keep country data
  reframe(.by = c(country, iso3, date), cases = sum(cases))

## daily region-level case data
cases_region_df <- cases_df |> 
  select(region = location, iso3 = iso_code, date, cases = new_cases, cases_moving_avg = new_cases_smoothed) |> 
  filter(str_detect(iso3, "OWID"), region != "World") |> # only keep region data
  mutate(date = floor_date(date, unit = "weeks")) |> 
  reframe(.by = c(region, iso3, date), cases = sum(cases))

# USA data 
## weekly mpox case data 
cases_weekly <- cases_daily |> 
  mutate(date = floor_date(date, unit = "weeks", week_start = 1)) |> ###
  reframe(.by = c(country, iso2, iso3, date), cases = sum(cases))


# Save data ====================================================================
# WHO data
write_csv(cases_country_df, here("3-data/mpox-cases/mpox-cases-countries.csv"))
write_csv(cases_region_df, here("3-data/mpox-cases/mpox-cases-regions.csv"))

# CDC data
write_csv(cases_daily, here("3-data/mpox-cases/mpox-cases-daily.csv"))
write_csv(cases_weekly, here("3-data/mpox-cases/mpox-cases-weekly.csv"))
