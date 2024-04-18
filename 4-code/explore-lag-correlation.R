# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================


### temp case case 
test_df <- mpox_df |>
  reframe(
    .by = c(country, iso2, iso3, date),
    pageviews = sum(pageviews),
    pct_pageviews = pageviews / pageviews_ceil,
    page_title = "Mpox",
    pageviews_ceil, 
    cases
  ) |> 
  distinct() 


# Test time-lag correlations between pageviews and cases =======================
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
  cor.test(clean_data$shifted_cases, clean_data[[outcome_var]], method = "spearman") |> 
    tidy() |> 
    #mutate(lag = lag) |> 
    rename(rho = estimate, S = statistic)
}


# lagged variable
lags <- -28:28
final_results <- data.frame(
  lag = integer(),
  rho = numeric(), # Assuming your function returns a column named rho for correlation coefficient
  S = numeric(), # Assuming S is the test statistic from your correlation test
  p_value = numeric(), # Assuming there's a p-value returned
  country = character(),
  stringsAsFactors = FALSE # Avoid factors for country names to simplify things
)

calculate_correlation_with_lag(data = test_df, outcome_var = "pct_pageviews", lagged_var = "cases", lag = -28)

for (title in unique(mpox_df$page_title)) {
  filtered_df <- mpox_df |> filter(page_title == title)
  results <- map(lags,
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

  final_results <- bind_rows(final_results, results)
}


# p-value
ggplot(final_results, aes(x = lag, y = p.value, color = country)) + 
  geom_line() + 
  facet_wrap(~country, scale = "free_y", ncol = 3) + 
  theme_minimal() +
  theme(legend.position = "none")

# Spearman coefficient
ggplot(final_results, aes(x = lag, y = rho, color = country)) + 
  geom_line() + 
  facet_wrap(~country, ncol = 3) + 
  theme_minimal() +
  theme(legend.position = "none")

# order countries
country_order <- final_results |> 
  select(country) |> 
  distinct() |> 
  left_join(iso_ref, by = join_by(country == country_name)) |> 
  left_join(World, by = join_by(country == name, iso3 == iso_a3)) |> 
  select(country, continent) |> 
  arrange(continent, country) |>  # could arrange by continet, number of cases
  pull(country)

# heatmap
final_results |> 
  mutate(country = factor(country, levels = country_order)) |> 
  ggplot(aes(x = lag, y = country, fill = rho)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#91bfdb", high = "#fc8d59") +
  scale_x_continuous(n.breaks = 20) +
  labs(
    title = "Time lag correlation of Wikipedia page views and weekly mpox cases",
    x = "Time lag [days]",
    y = NULL,
    fill = "Spearman \ncorrelation \ncoefficient"
  ) +
  theme_minimal()
 # TODO: Go back to weekly lags since I don't have that level of granularity


# correlations will contain the Spearman correlation coefficients for each lag
## negative lags correspond with the time-lag effect of new cases on pageviews
## positive lags correspond with the time-lag effect of pageviews on new cases 
