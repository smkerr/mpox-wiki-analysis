# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Author: Steve Kerr
# Date: April 2024
# ==============================================================================


# Setup ====================================================================
# Load packages
pacman::p_load(
  MASS,
  dplyr,
  glue,
  here,
  readr,
  stringr,
  install = FALSE
)


# Load data
cases_daily <- read_csv(here("3-data/mpox-cases/cdc-mpox-cases-daily.csv"))
cases_totals <- read_csv(here("3-data/mpox-cases/cdc-mpox-cases-totals.csv"))


# Prepare data =================================================================
cases_daily <- cases_daily |>  
  select(date = Epi_date_V3, cases = Cases, moving_avg_cases = `7-Day Average`) |> 
  mutate(country = "United States", iso2 = "US", iso3 = "USA", date = mdy(date)) |> 
  relocate(c(country, iso2, iso3), .before = everything())


# Save data ====================================================================
write_csv(cases_daily, here("3-data/mpox-cases/mpox-cases-daily.csv"))
