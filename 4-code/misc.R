# Misc.

# Lag correlation between cases and pageviews ==================================
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

# Define lags 
lags <- -25:25 ### TODO: Should be informed by segmented regression


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
