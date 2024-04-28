# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Author: Steve Kerr
# Date: April 2024
# ==============================================================================


# Setup ========================================================================
# Load packages
pacman::p_load(
  MASS, 
  dplyr,
  forcats,
  ggplot2,
  glue,
  here, 
  lubridate,
  readr, 
  scales,
  slider,
  spData,
  tmap,
  install = FALSE
)

# Load data
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

# Load lag analysis results
lag_results <- read_csv(here(glue("3-data/output/lag-analysis/lag-analysis-results.csv")))


# ISO ref table
load(here("3-data/ref/iso_codes.RData"))




# Lag Analysis =================================================================
summary_data <- lag_results |>
  group_by(page_title) |>
  mutate(page_title = factor(page_title, levels = rev(order_by_coefficients))) |> 
  summarise(
    Avg_Estimate = mean(estimate, na.rm = TRUE),
    Median_Estimate = median(estimate, na.rm = TRUE),
    Avg_P_Value = mean(p.value, na.rm = TRUE),
    Min_P_Value = min(p.value, na.rm = TRUE),
    Significant_Lags = sum(p.value < 0.05, na.rm = TRUE)
  )

summary_table <- gt(summary_data) |>
  tab_header(
    title = "Summary of Lag Analysis"
  ) |>
  cols_label(
    Avg_Estimate = "Average Estimate",
    Median_Estimate = "Median Estimate",
    Avg_P_Value = "Average P-Value",
    Min_P_Value = "Minimum P-Value",
    Significant_Lags = "Count of Significant Lags (p < 0.05)"
  ) |>
  fmt_number(
    columns = c(Avg_Estimate, Median_Estimate, Avg_P_Value, Min_P_Value),
    decimals = 2
  ) |>
  fmt_number(
    columns = c(Significant_Lags)
  ) |>
  tab_options(
    table.font.size = px(12),
    heading.background.color = "#D3D3D3"
  )

summary_table <- summary_table |>
  # Bold the page_title column
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(columns = c(page_title))
  ) |>
  # Bold the column headers
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(columns = everything())
  ) |>
  # Bold the table title
  tab_header(
    title = md("**Summary of Lag Analysis**")
  )


summary_table <- summary_table |>
  data_color(
    columns = c(Avg_Estimate, Median_Estimate),
    fn = scales::col_numeric(
      palette = c("lightblue", "darkblue"),
      domain = NULL  # Auto-scale based on the data range of each column
    )
  ) |>
  data_color(
    columns = c(Avg_P_Value, Min_P_Value),
    fn = scales::col_numeric(
      palette = c("lightgreen", "darkgreen"),
      domain = NULL
    )
  ) |>
  data_color(
    columns = c(Significant_Lags),
    fn = scales::col_numeric(
      palette = c("pink", "red"),
      domain = NULL
    )
  )
summary_table

# Save the table as HTML
gtsave(summary_table, here("5-tables/lag-analysis-summary-table.html"))