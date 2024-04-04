# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================

load(here("3-data/output/top-relevant-articles.RData"))


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


# Visualize R^2 ================================================================
# try out for countries with most cases
cases_1000 <- cases_df |> 
  reframe(.by = country, cases = sum(cases)) |> 
  filter(cases > 1000) |> 
  pull(country)

results_df |> 
  filter(country %in% cases_1000) |> # more than 1000 cases
  ggplot(aes(x = shift, y = r_squared, color = country)) + 
  geom_line() +
  facet_wrap(~country) + 
  labs(
    title = "Effect of Time Shift on Model Fit",
    x = "Shift Days", 
    y = "R^2 Value"
  ) +
  theme_minimal()
