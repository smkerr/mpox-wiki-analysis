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
source(here("code/funcs/helpers.R"))

# Define keywords --------------------------------------------------------------
source(here("code/scripts/01-define-keywords.R"))

# Wikipedia data ---------------------------------------------------------------
source(here("code/scripts/02-prepare-wiki-data.R"))

# Mpox case data ---------------------------------------------------------------
source(here("code/scripts/03-prepare-mpox-data.R"))

# Quality checks ---------------------------------------------------------------
#source(here("code/scripts/04-check_data_quality.R"))

# Data analysis ----------------------------------------------------------------
source(here("code/scripts/05-analyze_data.R"))
