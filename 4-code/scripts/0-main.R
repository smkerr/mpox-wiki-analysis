# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================


# Setup ========================================================================
# install dependencies
# remotes::install_github("wikimedia/waxer@main")

# load dependencies
pacman::p_load(
  MASS,         # statistical analysis
  broom,        # tidy model output
  dplyr,        # wrangle data
  forcats,      # factors
  ggplot2,      # visualize data
  glue,         # string literals
  gt,           # formatting tables
  here,         # file paths
  httr,         # API requests
  ISOcodes,     # ISO country codes
  janitor,      # tidy & explore data
  jsonlite,     # parse JSON
  lubridate,    # dates & times
  nlme,         # mixed-effects models
  nortest,      # normality tests
  purrr,        # vectorize operations
  readr,        # import data
  readxl,       # import excel files
  scales,       # scaling plots
  segmented,    # segmented regression analysis
  sf,           # simple features
  spData,       # spatial data
  stringr,      # strings
  tibble,       # tidy data frames
  tidyr,        # tidy data
  tmap,         # map-making
  tseries,      # time-series
  vars,         # vector autoregressive models
  waxer,        # Wikipedia pageviews
  WikipediR,    # Wikipedia links
  writexl,      # save excel files
  zoo           # rolling averages
)

# load custom functions
#source(here("4-code/funcs/helpers.R"))

# create reference table for ISO country codes
iso_ref <- ISO_3166_1 |> select(country_name = Name, iso2 = Alpha_2, iso3 = Alpha_3)
save(iso_ref, file = here("3-data/ref/iso_codes.RData"))

# Identify mpox-related articles ===============================================
source(here("4-code/scripts/1-identify-mpox-pages.R")) 

# Wikipedia data ===============================================================
source(here("4-code/scripts/2-prepare-wiki-data.R"))

# Mpox case data ===============================================================
source(here("4-code/scripts/3-prepare-mpox-data-v1.R"))

# Select most relevant articles ================================================
source(here("4-code/scripts/4-select-mpox-pages.R"))

# Merge data ===================================================================
source(here("4-code/scripts/5-merge-data.R"))


# Quality checks ===============================================================
#source(here("4-code/scripts/4-check-data-quality.R"))

# Data analysis ================================================================
#source(here("4-code/scripts/5-analyze-data.R"))

# Test forecasting potential ===================================================
source(here("4-code/scripts/5-test-forecasting-potential"))

