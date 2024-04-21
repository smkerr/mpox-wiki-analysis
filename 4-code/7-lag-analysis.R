# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================


# Setup ========================================================================
mpox_df <- read_csv(here("3-data/output/mpox-data.csv")) 


# Prepare data =================================================================
# create list of dates
date_sequence <- unique(mpox_df$date)

# cases by date
cases_by_date <- mpox_df |> distinct(date, cases)

# total monthly pageviews by date
total_pageviews_by_date <- mpox_df |> distinct(date, pageviews_ceil)

# expand to include missing dates
lag_df <- mpox_df |> 
  distinct(page_title, date) |> 
  complete(page_title, date = date_sequence) |> 
  left_join(mpox_df, by = join_by(page_title, date)) |> 
  group_by(page_title) |> 
  fill(country, iso2, iso3, project, wikidata_id, page_id, .direction = "updown") |> 
  ungroup() |> 
  select(-cases) |> 
  left_join(cases_by_date, by = join_by(date)) |> 
  select(-pageviews_ceil) |> 
  left_join(total_pageviews_by_date, join_by(date)) |> 
  select(country, iso2, iso3, project, wikidata_id, page_id, page_title, date, cases, pct_pageviews, pageviews, pageviews_ceil) |>
  group_by(page_title) |> 
  filter(sum(pageviews > 450, na.rm = TRUE) > 5) |> # at least 5 weeks of observations for a given article
  ungroup()

# lag_df1 <- mpox_df |>
#   # Combine "Monkeypox" + "Monkeypox virus" pageviews
#   filter(page_title %in% c("Monkeypox", "Monkeypox virus")) |> ###
#   reframe(
#     .by = c(country, iso2, iso3, date),
#     cases, 
#     pageviews = sum(pageviews, na.rm = TRUE),
#     pct_pageviews = pageviews / pageviews_ceil,
#     page_title = "Mpox"
#   ) |>
#   distinct() |> # remove duplicates
#   complete(
#     date = seq.Date(from = min(mpox_df$date), to = max(mpox_df$date), by = 1),
#     fill = list(country = "United States", iso2 = "US", iso3 = "USA", page_title = "Mpox")
#   ) 
# # TODO: Add 7-day rolling avg for cases
# # TODO: Add 7-day rolling avg for pct_pageviews


# lag_df2 <- mpox_df |>
#   select(date, country, iso2, iso3, cases, pageviews, pct_pageviews, page_title) |> 
#   filter(!page_title %in% c("Monkeypox", "Monkeypox virus")) |> 
#   bind_rows(lag_df1) # add combined "Monkeypox" + Monkeypox virus" pageviews
#  # TODO: Add 7-day rolling avg for cases
#  # TODO: Add 7-day rolling avg for pct_pageviews


# Function to calculate Spearman correlation with lag ==========================
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

# Lag correlations for "Mpox" pageviews ========================================
# TODO: Lags should be informed by attention decay
lags <- -25:25 # lag range #
mpox_results <- map(
  lags, 
  ~ calculate_correlation_with_lag(
    data = lag_df1, 
    outcome_var = "pct_pageviews", 
    lagged_var = "cases", 
    lag = .x)
  ) |> 
  bind_rows(.id = "lag") |> # combine lag correlations into single dataframe
  mutate(
  lag = as.integer(lags[as.integer(lag)]),
  page_title = "Mpox"
  )
# TODO: In cor.test.default(clean_data$shifted_cases, clean_data[[outcome_var]],  ... :
# Cannot compute exact p-value with ties

# p-value
ggplot(mpox_results, aes(x = lag, y = p.value)) + 
  geom_line() + 
  theme_minimal() +
  theme(legend.position = "none")

# Spearman coefficient
ggplot(mpox_results, aes(x = lag, y = estimate)) + 
  geom_line() + 
  theme_minimal() +
  theme(legend.position = "none")

# heatmap
mpox_results |> 
  ggplot(aes(x = lag, y = page_title, fill = estimate)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#91bfdb", high = "#fc8d59") +
  scale_x_continuous(n.breaks = 10) +
  labs(
    title = "Time lag correlation of Wikipedia page views and mpox cases",
    x = "Time lag [days]",
    y = NULL,
    fill = "Spearman \ncorrelation \ncoefficient"
  ) +
  theme_minimal()

# Spearman coefficient
ggplot(mpox_results, aes(x = lag, y = estimate, fill = estimate)) +
  geom_bar(stat = "identity") +
  facet_wrap(~page_title) +
  scale_fill_gradient2(low = "#91bfdb", mid = "#ffffbf", high = "#fc8d59", midpoint = 0, limits = c(-1, 1)) + 
  scale_x_continuous(n.breaks = 10) +
  theme_minimal() +
  labs(title = "Time-lag correlations of mpox-related Wikipedia pageviews and mpox cases",
       x = "Time lag [days]",
       y = "Correlation coefficient [r]")

# p-value
ggplot(mpox_results, aes(x = lag, y = p.value, fill = p.value)) +
  geom_bar(stat = "identity") +
  facet_wrap(~page_title) +
  scale_fill_gradient2(low = "#fc8d59", mid = "#ffffbf", high = "#91bfdb", midpoint = 0.5, limits = c(0, 1)) +
  scale_x_continuous(n.breaks = 10) +
  theme_minimal() +
  labs(title = "Time-lag correlations of mpox-related Wikipedia pageviews and mpox cases",
       x = "Time lag [days]",
       y = "Correlation coefficient [r]")


# correlations will contain the Spearman correlation coefficients for each lag
## negative lags correspond with the time-lag effect of new cases on pageviews
## positive lags correspond with the time-lag effect of pageviews on new cases 


# Lag correlations for mpox-related articles ===================================
# Initialize empty dataframe to store results
final_results <- data.frame()
for (title in unique(lag_df$page_title)) {
  filtered_df <- lag_df |> filter(page_title == title)
  results <- map(
    lags,
    ~ calculate_correlation_with_lag(
      data = filtered_df, 
      outcome_var = "pct_pageviews", 
      lagged_var = "cases", 
      lag = .x)
    ) |> 
    bind_rows(.id = "lag") |> # combine lag correlations into single dataframe
    mutate(
      lag = as.integer(lags[as.integer(lag)]),
      page_title = title
    )
  
  # TODO: 50: In cor.test.default(clean_data$shifted_cases, clean_data[[outcome_var]],  ... :
  #Cannot compute exact p-value with ties
  
  final_results <- bind_rows(final_results, results) |> 
    relocate(c(page_title, lag), .before = everything())
}















# Test time-lag correlations between online search activity & new cases ========
# Assuming 'du_df' is your data frame, 'Y' is your outcome variable, and 'cases_moving_avg' is your lagged variable
lags <- c(-21, -14, -7, 0, 7, 14, 21)
results <- purrr::map(lags, ~ calculate_correlation_with_lag(data = du_df, outcome_var = "Y", lagged_var = "cases_moving_avg", .x))

# Combine results into a single dataframe
final_results <- bind_rows(results, .id = "lag") %>% 
  mutate(lag = as.integer(lags[as.integer(lag)]))
final_results

# correlations will contain the Spearman correlation coefficients for each lag
## negative lags correspond with the time-lag effect of new cases on pageviews
## positive lags correspond with the time-lag effect of pageviews on new cases 










# Function to shift the pageviews and build the model
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
shifts <- -28:28 
# TODO: This value should be informed by attention decay

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