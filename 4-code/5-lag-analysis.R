# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Author: Steve Kerr
# Date: April 2024
# ==============================================================================


# Setup ========================================================================
# Load packages
pacman::p_load(
  MASS, 
  broom, 
  dplyr, 
  ggplot2, 
  glue, 
  gt,
  here, 
  lubridate, 
  purrr, 
  readr, 
  scales, 
  slider, 
  stringr,
  tibble,
  tidyr,
  install = FALSE
)

# Load data
mpox_df <- read_csv(here("3-data/output/mpox-data.csv")) |> 
  filter(date >= as_date("2022-05-10") & date <= as_date("2024-02-27")) 


# Prepare data =================================================================
lag_df <- left_join(
  # calculate 7-day rolling averages for pageviews 
  mpox_df |> 
    group_by(country, iso2, iso3, project, wikidata_id, page_id, page_title) |>
    filter(sum(!is.na(pct_pageviews), na.rm = TRUE) >= 30) |> # remove articles with <30 pageviews
    mutate(
      pct_pageviews = pageviews / pageviews_ceil,
      roll_pct_pageviews = slide_dbl(pct_pageviews, ~mean(.x, na.rm = TRUE), .before = 3, .after = 3)
    ) |> 
    ungroup() |> 
    mutate(across(everything(), ~replace(., is.nan(.), NA))) |> 
    select(-cases:-n_studies),
  # calculate 7-day rolling averages for cases, news articles, & scientific studies
  mpox_df |> 
    distinct(date, cases, n_articles, n_studies) |> 
    mutate(
      roll_cases = slide_dbl(cases, .f = ~mean(.x, na.rm = TRUE), .before = 3, .after = 3),
      roll_n_articles = slide_dbl(n_articles, .f = ~mean(.x, na.rm = TRUE), .before = 3, .after = 3),
      roll_n_studies = slide_dbl(n_studies, .f = ~mean(.x, na.rm = TRUE), .before = 3, .after = 3)
    ) |> 
    select(date, starts_with("roll")),
  by = join_by(date)
)

# Save data
write_csv(lag_df, here("3-data/output/lag-analysis/lag-analysis-data.csv"))


# Lag Correlation Function =====================================================
calculate_correlation_with_lag <- function(data, outcome_var, lagged_var, lag) {
  # Order chronologically
  data <- data |> arrange(date)
  
  # Shift the lagged variable manually
  if (lag > 0) {
    shifted_cases <- c(rep(NA, lag), data[[lagged_var]][1:(nrow(data) - lag)])
  } else if (lag < 0) {
    shifted_cases <- c(data[[lagged_var]][(-lag + 1):nrow(data)], rep(NA, -lag))
  } else { # lag == 0
    shifted_cases <- data[[lagged_var]]
  }
  
  # Remove rows where either variable is NA to ensure proper comparison
  clean_data <- data |> 
    mutate(shifted_cases = shifted_cases) |> 
    filter(!is.na(shifted_cases), !is.na(data[[outcome_var]]))
  
  # Perform the correlation test with the shifted data
  results <- try({
    cor.test(clean_data$shifted_cases, clean_data[[outcome_var]], method = "spearman") |> 
      tidy()
  }, silent = TRUE)
  
  if (inherits(results, "try-error")) {
    return(data.frame(estimate = NA, statistic = NA, p.value = NA, lag = lag))
  } else {
    return(results)
  }
}


# Calculate Correlation Across Lags ============================================
lags <- -35:35

lag_results <- data.frame()
for (title in unique(lag_df$page_title)) {
  filtered_df <- lag_df |> filter(page_title == title)
  results <- map(lags,
                 ~ calculate_correlation_with_lag(
                   data = filtered_df, 
                   outcome_var = "roll_pct_pageviews", 
                   lagged_var = "roll_cases", 
                   lag = .x)
                 ) |> 
    bind_rows(.id = "lag") |> # combine lag correlations into single dataframe
    mutate(
      lag = as.integer(lags[as.integer(lag)]),
      page_title = title
    ) 
  
  lag_results <- bind_rows(lag_results, results) |> 
    relocate(c(page_title, lag), .before = everything())
}

# Store results
write_csv(lag_results, here(glue("3-data/output/lag-analysis/lag-analysis-results.csv")))


# Select Most Relevant Articles ===============================================
# Filter based on Spearman correlation estimates
included_articles <- lag_results |> 
  mutate(estimate = abs(estimate)) |> 
  filter(estimate >= 0.5) |> # moderate to strong correlation
  group_by(page_title) |> 
  summarize(sum(p.value < 0.05) >= 7) |> # at least 7 lag periods where signif correlation
  ungroup() |> 
  pull(page_title) |> 
  unique()
  

# Save results =================================================================
save(included_articles, file = here("3-data/output/article-selection/mpox-pages-included.RData"))
