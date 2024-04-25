# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Author: Steve Kerr
# Date: April 2024
# ==============================================================================


# Setup ========================================================================
# Load dependencies
pacman::p_load(
  MASS,         # statistical analysis
  broom,        # tidy model output
  dotenv,       # env files
  dplyr,        # wrangle data
  forcats,      # factors
  forecast,     # forecasting
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
  pracma,       # numerical analysis
  purrr,        # vectorize operations
  readr,        # import data
  readxl,       # import excel files
  rentrez,      # query NCBI's EUtils API 
  scales,       # scaling plots
  segmented,    # segmented regression analysis
  sf,           # simple features
  slider,       # rolling windows
  spData,       # spatial data
  stringr,      # strings
  tibble,       # tidy data frames
  tidyr,        # tidy data
  tmap,         # map-making
  tseries,      # time-series
  vars,         # vector autoregressive models
  WikipediR,    # Wikipedia links
  writexl,      # save excel files
  xml2,         # XML
  zoo           # rolling averages
)

# Build reference tables
iso_ref <- ISO_3166_1 |> select(country_name = Name, iso2 = Alpha_2, iso3 = Alpha_3)
save(iso_ref, file = here("3-data/ref/iso_codes.RData"))

# Identify mpox-related Wikipedia articles =====================================
source(here("4-code/1-get-related-pages.R")) 

# Wikipedia data ===============================================================
source(here("4-code/2a-get-wiki-data.R"))

# Mpox case data ===============================================================
source(here("4-code/2b-get-case-data.R"))

# Mpox news coverage data ======================================================
source(here("4-code/2c-get-news-data.R"))

# Mpox scientific studies data =================================================
source(here("4-code/2d-get-studies-data.R"))

# Merge data ===================================================================
source(here("4-code/3-merge-data.R"))

# Measure attention decay ======================================================
source(here("4-code/4-attention-decay.R"))

# Test normality assumption ====================================================
source(here("4-code/5-test-normality.R"))

# Select most relevant Wikipedia articles ======================================
source(here("4-code/6-select-relevant-pages.R"))

# Lag analysis =================================================================
source(here("4-code/7-lag-analysis.R"))

# Regression analysis ==========================================================
#source(here("4-code/8-regression-analysis.R"))

# Test for Granger causality ===================================================
#source(here("4-code/9-granger-causality.R"))

# Tables & Figures =============================================================
#source(here("4-code/figures.R"))

# Calculations used in the paper ===============================================
#source(here("4-code/stats.R"))

#> All outputs can be found under `5-visualization` or `3-data/output`
