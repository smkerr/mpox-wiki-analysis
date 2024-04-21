# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================


# Test stationarity ============================================================
#> Implement ADF test to check for stationarity in both cases and pageviews.
#> Non-stationary data can lead to spurious results in subsequent analyses.

# check stationarity of pageviews time-series
adf.test(mpox_df$pageviews_est, alternative = "stationary")
# p-value > significance level of 0.05, so non-stationary
# still need to difference...

# check stationarity of mpox cases time-series
adf.test(mpox_df$cases_moving_avg, alternative = "stationary")
# p-value > significance level of 0.05, so non-stationary
# still need to difference...

# first-order differencing
mpox_df <- mpox_df |>
  mutate(
    pageviews_diff1 = c(NA, diff(pageviews_est, lag = 1, differences = 1)),
    cases_moving_avg_diff1 = c(NA, diff(cases_moving_avg, lag = 1, differences = 1))
  )

# plot first-order differenced pageviews
mpox_df |>
  ggplot(aes(x = date, y = pageviews_diff1)) +
  geom_line() +
  labs(
    title = "Daily pageviews for mpox-related pages",
    #subtitle = country_name,
    x = NULL,
    y = "Views",
    caption = "Source: World Health Organization via Our World in Data"
  ) +
  theme_minimal()

# plot first-order differenced cases
mpox_df |>
  ggplot(aes(x = date, y = cases_moving_avg_diff1)) +
  geom_line() +
  labs(
    title = "Daily mpox cases",
    #subtitle = country_name,
    x = NULL,
    y = "Cases",
    caption = "Source: Wikimedia Foundation"
  ) +
  theme_minimal()

# check stationarity of first-order differenced pageviews time-series
mpox_df |>
  filter(!is.na(pageviews_diff1)) |>
  pull(pageviews_diff1) |>
  adf.test(alternative = "stationary")
# p-value < significance level of 0.05, so stationary

# check stationarity of first-order differenced mpox cases time-series
mpox_df |>
  filter(!is.na(cases_moving_avg_diff1)) |>
  pull(cases_moving_avg_diff1) |>
  adf.test(alternative = "stationary")
# p-value < significance level of 0.05, so stationary


# Develop VAR model ============================================================
#> Develop a VAR model to understand the dynamic relationship between the two time series. This model will help in capturing the temporal interdependencies and feedback mechanisms between Wikipedia traffic and mpox cases.

# convert to a time-series object
mpox_ts <- mpox_df |>
  filter(!is.na(pageviews_diff1), !is.na(cases_moving_avg_diff1)) |>
  select(pageviews_diff1, cases_moving_avg_diff1) |>
  ts()

# determine optimal lag length for VAR model based on information criteria (e.g., AIC)
lag_selection <- VARselect(mpox_ts, lag.max = 24, type = "both")
optimal_lag <- lag_selection$selection["AIC(n)"]

# fit VAR model
var_model <- VAR(mpox_ts, p = optimal_lag, type = "both")
var_model

# summary of VAR model
summary(var_model)


# Apply Granger causality test =================================================
#> Apply the Granger causality test within the VAR framework to assess whether Wikipedia traffic volumes can be considered a predictor of mpox case trajectories.

# test if pageviews Granger-cause mpox cases
granger_pageviews <- causality(var_model, cause = "pageviews_diff1")
granger_pageviews

# test if mpox cases Granger-cause mpox cases
granger_cases <- causality(var_model, cause = "cases_moving_avg_diff1")
granger_cases

#> The key aspect of the result to look at is the p-value. If the p-value is less than your significance level (commonly set at 0.05), you can conclude that there is evidence to suggest that the cause variable Granger-causes the other variable(s) in the VAR model

