# Script to perform data analysis for thesis topic 

# Setup ------------------------------------------------------------------------
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
  #haven
  here,         # file paths
  httr,         # API requests
  janitor,      # tidy & explore data
  jsonlite,     # parse JSON
  lubridate,    # dates & times
  #pageviews
  purrr,        # vectorize operations
  #RColorBrewer
  readr,        # import data
  readxl,       # import excel files
  scales,       # scaling plots
  stringr,      # strings
  tibble,       # tidy data frames
  tidyr,        # tidy data
  tseries,      # time-series
  vars,         # vector autoregressive models
  waxer,        # Wikipedia pageviews
  WikipediR,    # Wikipedia links
  #wikipediatrend
  writexl,      # save excel files
  zoo           # rolling averages
)

# load custom functions
source(here("4-code/funcs/helpers.R"))

# Define keywords --------------------------------------------------------------
#source(here("code/scripts/01-define-keywords.R")) # Simplify: "mpox" and "monkeypox virus"

# Wikipedia data ---------------------------------------------------------------
source(here("4-code/scripts/2-prepare-wiki-data.R"))

# Mpox case data ---------------------------------------------------------------
source(here("4-code/scripts/3-prepare-mpox-data.R"))

# Quality checks ---------------------------------------------------------------
#source(here("4-code/scripts/4-check_data_quality.R"))

# Data analysis ----------------------------------------------------------------
source(here("4-code/scripts/5-analyze_data.R"))
