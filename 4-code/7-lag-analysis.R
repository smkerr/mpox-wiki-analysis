# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================


# Setup ========================================================================
# Load data
mpox_df <- read_csv(here("3-data/output/mpox-data.csv")) 

# Included articles
load(here("3-data/output/mpox-pages-included.RData"))


# Prepare data =================================================================
# create list of dates
date_sequence <- unique(mpox_df$date)

# cases by date
cases_by_date <- mpox_df |> distinct(date, cases)

# total monthly pageviews by date
total_pageviews_by_date <- mpox_df |> distinct(date, pageviews_ceil)

# expand to include missing dates
lag_df <- mpox_df |> 
  filter(page_title %in% included_articles) |> # only include relevant articles 
  distinct(page_title, date) |> # get all combinations of articles and dates
  complete(page_title, date = date_sequence) |> # expand to include all dates from study period
  left_join(mpox_df, by = join_by(page_title, date)) |> # combine with existing data 
  group_by(page_title) |> 
  fill(country, iso2, iso3, project, wikidata_id, page_id, .direction = "updown") |> # fill missing info
  ungroup() |> 
  select(-cases) |> 
  left_join(cases_by_date, by = join_by(date)) |> # combine with case data
  select(-pageviews_ceil) |> 
  left_join(total_pageviews_by_date, join_by(date)) |> # combine with monthly total pageview data 
  select(country, iso2, iso3, project, wikidata_id, page_id, page_title, date, cases, pct_pageviews, pageviews, pageviews_ceil) 
# TODO: Add 7-day rolling avg for cases
# TODO: Add 7-day rolling avg for pct_pageviews
# TODO: Decide whether to combine "Monkeypox" + "Monkeypox virus" or keep separate


# Calculate time-lagged correlation between cases and pageviews ================
# Function to calculate Spearman correlation with lag
calculate_correlation_with_lag <- function(data, outcome_var, lagged_var, lag) {
  
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

# Lag correlation for pageviews by article =====================================
# Define lags 
lags <- -25:25 ###

# Initialize empty dataframe to store results
lag_results <- data.frame()

for (title in unique(lag_df$page_title)) {
  filtered_df <- mpox_df |> filter(page_title == title)
  results <- map(lags,
                 ~ calculate_correlation_with_lag(
                   data = filtered_df, 
                   outcome_var = "pct_pageviews", 
                   lagged_var = "cases", # TODO: Should cases be logged???
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

# TODO: 50: In cor.test.default(clean_data$shifted_cases, clean_data[[outcome_var]],  ... :
# Cannot compute exact p-value with ties

# Visualize lag correlations for pageviews by article ==========================
# Spearman coefficient
lag_results |> 
  ggplot(aes(x = lag, y = estimate, color = page_title)) + 
  geom_line() + 
  facet_wrap(~page_title, nrow = 1) +
  scale_y_continuous(limits = c(-1, 1)) +
  theme_minimal() +
  theme(legend.position = "none")

# p-value
lag_results |> 
  ggplot(aes(x = lag, y = p.value, color = page_title)) + 
  geom_line() + 
  facet_wrap(~page_title, ncol = 1) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal() +
  theme(legend.position = "none")

# Order articles by average lag value of max coefficient
order_by_coefficients <- lag_results |> 
  group_by(page_title) |> 
  slice_max(estimate) |> 
  arrange(-lag) |> 
  pull(page_title)

# # Order articles by average Spearman correlation coefficient
# order_by_coefficients <- final_results |> 
#   reframe(
#     .by = page_title,
#     avg_rho = mean(rho)
#   ) |> 
#   arrange(avg_rho) |> 
#   pull(page_title)

# heatmap of Spearman coefficient
lag_results |>
  mutate(page_title = factor(page_title, levels = order_by_coefficients)) |> 
  ggplot(aes(x = lag, y = page_title, fill = estimate, color = page_title)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#91bfdb", mid = "#ffffbf", high = "#fc8d59",  midpoint = 0, limits = c(-1, 1)) +
  scale_x_continuous(n.breaks = 10) +
  labs(
    title = "Time lag correlation of Wikipedia page views and mpox cases",
    x = "Time lag [days]",
    y = NULL,
    fill = "Spearman \ncorrelation \ncoefficient"
  ) +
  theme_minimal()

# # Order articles by average p-value
# order_by_p.values <- lag_results |> 
#   reframe(
#     .by = page_title,
#     avg_p.value = mean(p.value)
#   ) |> 
#   arrange(-avg_p.value) |> 
#   pull(page_title)

# Order articles by lag value of min p-value
order_by_p.values <- lag_results |> 
group_by(page_title) |> 
  slice_max(p.value) |> 
  arrange(lag) |> 
  pull(page_title)

# heatmap of p-values
lag_results |> 
  mutate(page_title = factor(page_title, levels = order_by_p.values)) |> 
  ggplot(aes(x = lag, y = page_title, fill = p.value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    limits = c(0, 1),
    midpoint = 0.5,
    low = "#fc8d59",
    mid = "#ffffbf",
    high = "#91bfdb"
  ) +
  scale_x_continuous(n.breaks = 10) +
  labs(
    title = "Significance of time lag correlation of Wikipedia page views and mpox cases",
    x = "Time lag [days]",
    y = NULL,
    fill = "p-value"
  ) +
  theme_minimal()

# Spearman coefficient
ggplot(lag_results, aes(x = lag, y = estimate, fill = estimate)) +
  geom_bar(stat = "identity") +
  facet_wrap(~page_title, ncol = 1) +
  scale_fill_gradient2(low = "#91bfdb", mid = "#ffffbf", high = "#fc8d59", midpoint = 0, limits = c(-1, 1)) + 
  scale_x_continuous(n.breaks = 10) +
  theme_minimal() +
  labs(title = "Time-lag correlations of mpox-related Wikipedia pageviews and mpox cases",
       x = "Time lag [days]",
       y = "Correlation coefficient [r]")

# p-value
ggplot(lag_results, aes(x = lag, y = p.value, fill = p.value)) +
  geom_bar(stat = "identity") +
  facet_wrap(~page_title, ncol = 1) +
  scale_fill_gradient2(low = "#fc8d59", mid = "#ffffbf", high = "#91bfdb", midpoint = 0.5, limits = c(0, 1)) +
  scale_x_continuous(n.breaks = 10) +
  theme_minimal() +
  labs(title = "Time-lag correlations of mpox-related Wikipedia pageviews and mpox cases",
       x = "Time lag [days]",
       y = "Correlation coefficient [r]")

# correlations will contain the Spearman correlation coefficients for each lag
## negative lags correspond with the time-lag effect of new cases on pageviews
## positive lags correspond with the time-lag effect of pageviews on new cases 


# Test time-lag correlations between online search activity & new cases ========
# Function to shift pageviews and build the model
shift_and_model <- function(df, shift_days) {
  # Order chronologically
  df <- df |> 
    arrange(date)
  
  # Independent variables
  independent_vars <- names(df)[!names(df) %in% c("country", "iso2", "iso3", "cases", "date", "cases_moving_avg")]
  formula_str <- paste("cases_moving_avg ~", paste(independent_vars, collapse = " + "))
  model_formula <- as.formula(formula_str)
  
  # Decide to use lead() or lag() based on the sign of shift_days
  if (shift_days > 0) {
    shifted_df <- df |> 
      mutate(across(all_of(independent_vars), ~lead(.x, abs(shift_days)))) |> 
      drop_na()
  } else {
    shifted_df <- df |> 
      mutate(across(all_of(independent_vars), ~lag(.x, abs(shift_days)))) |> 
      drop_na()
  }
  
  # Build the model
  model <- lm(model_formula, data = shifted_df)
  
  # Return model metrics
  return(glance(model)$r.squared) 
}

# Range of shifts from 28 days forward to 28 days backward
shifts <- lags 

# prepare data
mpox_ts <- mpox_df |>
  inner_join(top_mpox_pages, by = join_by(country, iso2, iso3, page_title)) |> 
  select(-est_pageviews, -pageviews_ceil, -correlation) |>
  pivot_wider(names_from = page_title, values_from = pct_est_pageviews, names_prefix = "pv_") |>
  reframe(
    .by = c(country, iso2, iso3, date, cases, cases_moving_avg),
    across(starts_with("pv_"), ~sum(.x, na.rm = TRUE))
  ) |> 
  group_by(country)

# Initialize list to store results
results <- list()

# Iterate over each country within the prepared data
for (country_name in unique(mpox_ts$country)) {
  country_data <- filter(mpox_ts, country == country_name)
  
  # Initialize a data frame to store R^2 values for this country
  country_results <- tibble(shift = integer(), r_squared = numeric())
  
  # Loop over each shift value and build the model for this country
  for (shift_days in shifts) {
    r_squared <- shift_and_model(country_data, shift_days)
    country_results <- bind_rows(country_results, tibble(shift = shift_days, r_squared = r_squared))
  }
  
  # Store the results for this country
  results[[country_name]] <- country_results
}

# combine into df
results_df <- bind_rows(results, .id = "country") |> 
  left_join(iso_ref, by = join_by(country == country_name)) |> 
  relocate(c(iso2, iso3), .after = country)

# Store results
for (iso3_code in unique(results_df$iso3)) {
  # R^2 for all shifts
  results_df |> 
    filter(iso3 == iso3_code) |> 
    write_csv(here(glue("3-data/output/regression-results/regression-results-{iso3_code}.csv")))
}

# Calculate highest R^2 values
results_df |> 
  group_by(country) |> 
  slice_max(r_squared) |> 
  ungroup() |> 
  write_csv(here(glue("3-data/output/regression-results/top-rsquared.csv")))