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
  gt,
  here, 
  lubridate,
  readr, 
  scales,
  slider,
  spData,
  stringr,
  tidyr,
  tmap,
  install = FALSE
)

# Load lag analysis results
lag_results <- read_csv(here(glue("3-data/output/lag-analysis/lag-analysis-results.csv")))

# Regression analysis results
model_summary_table <- read_csv(here("3-data/output/regression-analysis/regression-models-summary.csv"))
coef_summary_table <- read_csv(here("3-data/output/regression-analysis/regression-coef-summary.csv"))

# ISO ref table
load(here("3-data/ref/iso_codes.RData"))


# Lag Analysis =================================================================
lag_results |> 
  select(lag, page_title, estimate) |> 
  pivot_wider(names_from = page_title, values_from = estimate) |> 
  gt()

summary_data <- lag_results |>
  group_by(page_title) |>
  summarise(
    Avg_Estimate = mean(estimate, na.rm = TRUE),
    Median_Estimate = median(estimate, na.rm = TRUE),
    Avg_P_Value = mean(p.value, na.rm = TRUE),
    Median_P_Value = median(p.value, na.rm = TRUE),
    Significant_Lags = sum(p.value < 0.05, na.rm = TRUE)
  )

summary_table <- gt(summary_data) |>
  tab_header(
    title = "Summary of Lag Analysis"
  ) |>
  cols_label(
    page_title = "Page Title",
    Avg_Estimate = "Average Estimate",
    Median_Estimate = "Median Estimate",
    Avg_P_Value = "Average P-Value",
    Median_P_Value = "Median P-Value",
    Significant_Lags = "Count of Significant Lags (p < 0.05)"
  ) |>
  fmt_number(
    columns = c(Avg_Estimate, Median_Estimate, Avg_P_Value, Median_P_Value),
    decimals = 2
  ) |>
  fmt_number(
    columns = c(Significant_Lags),
    decimals = 0
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

gtsave(summary_table, here("5-tables/lag-analysis-summary-table.tex"))


summary_table <- summary_table |>
  data_color(
    columns = c(Avg_Estimate, Median_Estimate),
    fn = scales::col_numeric(
      palette = c("lightblue", "darkblue"),
      domain = NULL  # Auto-scale based on the data range of each column
    )
  ) |>
  data_color(
    columns = c(Avg_P_Value, Median_P_Value),
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


# Regression Analysis ==========================================================
## Training Data ---------------------------------------------------------------
# Goodness of fit summary 
gt_model_summary_table <- model_summary_table |> 
  gt() |>
  tab_header(
    title = "Goodness-of-fit statistics for different multivariate regression models",
  ) |>
  fmt_number(
    columns = c(mpox, included_articles, mpox_covars, included_articles_covars),
    rows = c(1:2),
    decimals = 2
  ) |>
  fmt_number(
    columns = c(mpox, included_articles, mpox_covars, included_articles_covars),
    rows = c(3:4),
    decimals = 0
  ) |>
  cols_label(
    mpox = 'Mpox pageviews',
    included_articles = "Mpox-related pageviews",
    mpox_covars = 'Mpox pageviews + media coverage + scientific interest',
    included_articles_covars = "Mpox-related pageviews + media coverage + scientific interest",
  ) |> 
  cols_align(
    align = "center",
    columns = c(mpox, included_articles, mpox_covars, included_articles_covars)
  ) 
gt_model_summary_table

# Save the gt table as a LaTeX file
gtsave(gt_model_summary_table, here("5-tables/regression-models-summary-table.tex"))


# Coefficient Summary
gt_coef_summary_table <- coef_summary_table |>
  mutate(
    term = case_when(
      !term %in% c("(Intercept)", "roll_n_articles", "roll_n_studies") ~ glue("\"{str_remove_all(term, '`')}\" pageviews"), 
      term == "(Intercept)" ~ "Intercept", 
      term == "roll_n_articles" ~ "GNews",
      term == "roll_n_studies" ~ "PubMed"
    ),
    Model = case_when(
      Model == "mpox" ~ 'Mpox pageviews',
      Model == "included_articles" ~ "Mpox-related pageviews",
      Model == "mpox_covars" ~ 'Mpox pageviews + media coverage + scientific interest',
      Model == "included_articles_covars" ~ "Mpox-related pageviews + media coverage + scientific interest",
    ),
    p.value = case_when(
      p.value < 0.001 ~ "<0.001",
      TRUE ~ as.character(round(p.value, 3))
    )
  ) |> 
  gt(groupname_col = "Model", rowname_col = "term") |>
  fmt_number(
    columns = c(estimate, std.error),
    decimals = 0
  ) |> 
  fmt_number(
    columns = c(statistic, p.value),
    decimals = 2
  ) |> 
  cols_label(
    estimate = "Value",
    std.error = "SE",
    statistic = "T", 
    p.value = "p-value"
  ) |>
  tab_header(title = "Multivariate regression models estimating the impact of different predictors") |>
  cols_align(align = "center", columns = term:p.value)
gt_coef_summary_table

# Save the gt table as a LaTeX file
gtsave(gt_coef_summary_table, filename = here("5-tables/regression-coef-summary-table.tex"))
