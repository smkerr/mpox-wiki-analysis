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
  tidyr
)

# Load data
mpox_df <- read_csv(here("3-data/output/mpox-data.csv")) |> 
  filter(date >= as_date("2022-05-10") & date <= as_date("2023-02-05")) 


# Prepare data =================================================================
mpox_df <- left_join(
  # calculate 7-day rolling averages for pageviews 
  mpox_df |> 
    filter(sum(pct_pageviews > 0, na.rm = TRUE) > 1) |> # remove articles with zero pageviews
    group_by(country, iso2, iso3, project, wikidata_id, page_id, page_title) |> 
    mutate(
      pageviews = ifelse(is.na(pageviews), 450, pageviews), # set missing values equal to lower bound threshold
      pct_pageviews = pageviews / pageviews_ceil,
      roll_pct_pageviews = slide_dbl(pct_pageviews, ~mean(.x, na.rm = TRUE), .before = 6)
    ) |> 
    ungroup() |> 
    mutate(across(everything(), ~replace(., is.nan(.), NA))) |> 
    select(-cases:-n_studies),
  # calculate 7-day rolling averages for cases, news articles, & scientific studies
  mpox_df |> 
    distinct(date, cases, n_articles, n_studies) |> 
    mutate(
      roll_cases = slide_dbl(cases, .f = ~mean(.x, na.rm = TRUE), .before = 6),
      roll_n_articles = slide_dbl(n_articles, .f = ~mean(.x, na.rm = TRUE), .before = 6),
      roll_n_studies = slide_dbl(n_studies, .f = ~mean(.x, na.rm = TRUE), .before = 6)
    ) |> 
    select(date, starts_with("roll")),
  by = join_by(date)
)

# Save data
write_csv(mpox_df, here("3-data/output/processed_mpox_data.csv"))


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
lags <- -37:37

lag_results <- data.frame()
for (title in unique(mpox_df$page_title)) {
  filtered_df <- mpox_df |> filter(page_title == title)
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
write_csv(lag_results, here(glue("3-data/output/lag-analysis-results.csv")))


# Evaluate Results =============================================================
## Visualize coefficient estimates ---------------------------------------------
# Order articles by average lag value of max coefficient
order_by_coefficients <- lag_results |> 
  reframe(.by = page_title, avg_estimate = mean(estimate, na.rm = TRUE)) |> 
  arrange(avg_estimate) |> 
  pull(page_title)

# Plot heatmap of Spearman coefficients
lag_results |>
  mutate(page_title = factor(page_title, levels = order_by_coefficients)) |> 
  ggplot(aes(x = lag, y = page_title, fill = estimate, color = page_title)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#91bfdb", mid = "#ffffbf", high = "#fc8d59",  midpoint = 0, limits = c(-1, 1)) +
  scale_x_continuous(n.breaks = 15) +
  labs(
    title = "Time lag correlation of Wikipedia page views and mpox cases",
    x = "Time lag [days]",
    y = NULL,
    fill = "Spearman \ncorrelation \ncoefficient"
  ) +
  theme_minimal()

# Save image
ggsave(filename = here("5-visualization/spearman-correlation-heatmap.png"), width = 10, height = 8, dpi = 300)


# Plot barplot of Spearman coefficients
lag_results |> 
  mutate(page_title = factor(page_title, levels = order_by_coefficients)) |> 
  ggplot(aes(x = lag, y = estimate, fill = estimate)) +
  geom_bar(stat = "identity") +
  facet_wrap(~page_title, ncol = 4) +
  scale_fill_gradient2(
    low = "#91bfdb",
    mid = "#ffffbf", 
    high = "#fc8d59", 
    midpoint = 0, 
    limits = c(-1, 1)
    ) + 
  scale_x_continuous(n.breaks = 15) +
  theme_minimal() +
  labs(
    title = "Time-lag correlations of mpox-related Wikipedia pageviews and mpox cases",
     x = "Time lag [days]",
      y = "Correlation coefficient [r]"
    )

# Save image
ggsave(filename = here("5-visualization/spearman-correlation-barplot.png"), width = 10, height = 8, dpi = 300)


## Visualize p-values ----------------------------------------------------------
# Order articles by lag value of min p-value
order_by_p.values <- lag_results |> 
  group_by(page_title) |> 
  slice_max(p.value) |> 
  arrange(lag) |> 
  pull(page_title)

# Plot heatmap of p-values
lag_results |> 
  mutate(
    signif = case_when(
      p.value < 0.001 ~ "***", 
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ "Not signif."),
    signif = factor(signif, levels = c("Not signif.", "*", "**", "***")),
    page_title = factor(page_title, levels = order_by_p.values)
    ) |> 
  ggplot(aes(x = lag, y = page_title, fill = signif)) +
  geom_tile(color = "white") +
  scale_fill_brewer(
    type = "seq",
    palette = 4 
  ) +
  scale_x_continuous(n.breaks = 15) +
  labs(
    title = "Significance of time lag correlation of Wikipedia page views and mpox cases",
    x = "Time lag [days]",
    y = NULL,
    fill = "Significance\nlevel"
  ) +
  theme_minimal()

# Save image
ggsave(filename = here("5-visualization/spearman-pvalues-heatmap.png"), width = 10, height = 8, dpi = 300)

# Plot barplots of p-values
lag_results |> 
  ggplot(aes(x = lag, y = p.value, fill = p.value)) +
  geom_bar(stat = "identity") +
  facet_wrap(~page_title, ncol = 4) +
  scale_fill_gradient2(
    low = "#fc8d59",
    mid = "#ffffbf", 
    high = "#91bfdb",
    midpoint = 0.5, 
    limits = c(0, 1)
    ) +
  scale_x_continuous(n.breaks = 15) +
  theme_minimal() +
  labs(
    title = "Time-lag correlations of mpox-related Wikipedia pageviews and mpox cases",
    x = "Time lag [days]",
    y = "Correlation coefficient [r]"
    )

# Save image
ggsave(filename = here("5-visualization/spearman-pvalues-barplot.png"), width = 10, height = 8, dpi = 300)

# Summary Table ================================================================
summary_data <- lag_results %>%
  group_by(page_title) %>%
  mutate(page_title = factor(page_title, levels = rev(order_by_coefficients))) |> 
  summarise(
    Avg_Estimate = mean(estimate, na.rm = TRUE),
    Median_Estimate = median(estimate, na.rm = TRUE),
    Avg_P_Value = mean(p.value, na.rm = TRUE),
    Min_P_Value = min(p.value, na.rm = TRUE),
    Significant_Lags = sum(p.value < 0.05, na.rm = TRUE)
  )

summary_table <- gt(summary_data) %>%
  tab_header(
    title = "Summary of Lag Analysis"
  ) %>%
  cols_label(
    Avg_Estimate = "Average Estimate",
    Median_Estimate = "Median Estimate",
    Avg_P_Value = "Average P-Value",
    Min_P_Value = "Minimum P-Value",
    Significant_Lags = "Count of Significant Lags (p < 0.05)"
  ) %>%
  fmt_number(
    columns = c(Avg_Estimate, Median_Estimate, Avg_P_Value, Min_P_Value),
    decimals = 2
  ) %>%
  fmt_number(
    columns = c(Significant_Lags)
  ) %>%
  tab_options(
    table.font.size = px(12),
    heading.background.color = "#D3D3D3"
  )

summary_table <- summary_table %>%
  # Bold the page_title column
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(columns = c(page_title))
  ) %>%
  # Bold the column headers
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(columns = everything())
  ) %>%
  # Bold the table title
  tab_header(
    title = md("**Summary of Lag Analysis**")
  )


summary_table <- summary_table %>%
  data_color(
    columns = c(Avg_Estimate, Median_Estimate),
    colors = scales::col_numeric(
      palette = c("lightblue", "darkblue"),
      domain = NULL  # Auto-scale based on the data range of each column
    )
  ) %>%
  data_color(
    columns = c(Avg_P_Value, Min_P_Value),
    colors = scales::col_numeric(
      palette = c("lightgreen", "darkgreen"),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(Significant_Lags),
    colors = scales::col_numeric(
      palette = c("pink", "red"),
      domain = NULL
    )
  )
summary_table

# Save the table as HTML
gtsave(summary_table, here("5-visualization/lag-analysis-summary-table.html"))

# Conclusion ===================================================================
#> Negative lags correspond with the time-lag effect of independent vars on cases
#> Positive lags correspond with the time-lag effect of cases on independent vars 
