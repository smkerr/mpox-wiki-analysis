# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Author: Steve Kerr
# Date: April 2024
# ==============================================================================


# Load data ====================================================================
# TODO: I may not actually need all of these datasets
# load combined data
mpox_df <- read_csv(here("3-data/output/mpox-data.csv"))

# load pageviews data
pageviews <- read_csv(here("3-data/wikipedia/pageviews-differential-private.csv"))
pageviews_daily <- read_csv(here("3-data/wikipedia/pageviews-daily.csv"))
pageviews_weekly <- read_csv(here("3-data/wikipedia/pageviews-weekly.csv"))
pageviews_total <- read_csv(here("3-data/wikipedia/total-pageviews.csv"))

# load mpox case data
## WHO data
cases_country_df <- read_csv(here("3-data/mpox-cases/mpox-cases-countries.csv"))
cases_region_df <- read_csv(here("3-data/mpox-cases/mpox-cases-regions.csv"))
## CDC data
cases_daily <- read_csv(here("3-data/mpox-cases/mpox-cases-daily.csv"))
cases_weekly <- read_csv(here("3-data/mpox-cases/mpox-cases-weekly.csv"))

# load media coverage data
news_df <- read_csv(here("3-data/mpox-news/mpox-total-articles-deduplicated.csv"))

# load academic interest data
studies_df <- read_csv(here("3-data/mpox-studies/mpox-total-studies.csv"))

# ISO ref table
load(here("3-data/ref/iso_codes.RData"))