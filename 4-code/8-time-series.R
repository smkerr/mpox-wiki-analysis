# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================


# Setup ========================================================================
# Load data 
mpox_df <- read_csv(here("3-data/output/mpox-data.csv"))


# Prepare data =================================================================
mpox_ts <- mpox_df |> 
  filter(date >= as_date("2022-05-01")) |> 
  filter(page_title == "Mpox") |> 
  complete(date = seq.Date(from = min(as_date("2022-05-01")), to = max(mpox_df$date), by = 1)) |> # fill in missing dates
  complete(fill = list(cases = 0, pageviews = 450, n_articles = 0, n_studies = 0)) |> # impute NAs (set missing pageviews equal to threshold)
  fill(country, iso2, iso3, project, wikidata_id, page_id, page_title, pageviews_ceil, .direction = "downup") |> 
  # calculate rolling averages to smooth values
  mutate(
    pct_pageviews = pageviews / pageviews_ceil,
    ma_pct_pageviews = slide_dbl(pct_pageviews, ~mean(.x, na.rm = TRUE), .before = 6, .complete = FALSE), 
    ma_cases = slide_dbl(cases, .f = ~mean(.x, na.rm = TRUE), .before = 6),
    ma_n_articles = slide_dbl(n_articles, .f = ~mean(.x, na.rm = TRUE), .before = 6),
    ma_n_studies = slide_dbl(n_studies, .f = ~mean(.x, na.rm = TRUE), .before = 6)
  ) |> 
  select(-cases, -n_articles, -n_studies, -pageviews, -pageviews_ceil, -pct_pageviews)

# Viz check
mpox_ts |> 
  pivot_longer(cols = starts_with("ma"), names_to = "var", values_to = "value") |> 
  ggplot(aes(x = date, y = value, color = var)) +
  geom_line() +
  facet_wrap(~var, ncol = 1, scales = "free_y") +
  scale_x_date(date_labels = "%b\n%Y") +
  labs(title = "Time Series Plot", x = NULL, y = "Value") +
  theme_minimal()

# Test for stationarity ========================================================
#> Implement ADF test to check for stationarity in all included variables.
#> Non-stationary data can lead to spurious results in subsequent analyses.
adf_results1 <- mpox_ts |> 
  select(starts_with("ma")) |> 
  map(adf.test, alternative = "stationary")
adf_results1 <- adf_results1[1:4] |> 
  map_dfr(tidy) |> 
  mutate(var = names(adf_results1[1:4])) |> 
  relocate(var, .before = everything())
non_stationary1 <- adf_results1 |> 
  filter(p.value > 0.05)

# First-order differencing to achieve stationarity
if (nrow(non_stationary1) > 0) {
  # drop first observation
  mpox_ts1 <- mpox_ts[-1, ]
  
  # apply first-order differencing to non-stationary vars
  for (var in non_stationary1$var) {
    mpox_ts1[[var]] <- diff(mpox_ts[[var]], differences = 1)
  }
}

# Viz check
mpox_ts1 |> 
  pivot_longer(cols = starts_with("ma"), names_to = "var", values_to = "value") |> 
  ggplot(aes(x = date, y = value, color = var)) +
  geom_line() +
  facet_wrap(~var, ncol = 1, scales = "free_y") +
  scale_x_date(date_labels = "%b\n%Y") +
  labs(title = "Time Series Plot", x = NULL, y = "Value") +
  theme_minimal()

# Check for stationarity 
adf_results2 <- mpox_ts1 |> 
  select(starts_with("ma")) |> 
  map(adf.test, alternative = "stationary")
adf_results2 <- adf_results2[1:4] |> 
  map_dfr(tidy) |> 
  mutate(var = names(adf_results2[1:4])) |> 
  relocate(var, .before = everything())
non_stationary2 <- adf_results2 |> 
  filter(p.value > 0.05)
#> p-values of ADF test for all variables are <0.05, therefore we can consider
#> each variable to be stationary


# Determine optimal lag length =================================================
library(vars)
# Determine the optimal number of lags based on information criteria (e.g., AIC)
lag_selection <- mpox_ts1 |> 
  as.data.frame() |> 
  select(starts_with("ma")) |> 
  VARselect(lag.max = 10, type = "both") # TODO: WHY IS THE OPTIMAL NUMBER OF LAGS ALWAYS THE HIGHEST OPTION?
optimal_lag <- lag_selection$selection["AIC(n)"]


# Convert to a time-series object ==============================================
ts_df <- mpox_ts1 |> 
  select(starts_with("ma")) |> 
  ts()


# Fit VAR model ================================================================
var_model <- VAR(ts_df, p = optimal_lag, type = "both")
var_model

# Summary of VAR model
summary(var_model)


# Validate the model ===========================================================
# Check for autocorrelation in residuals
#> Check whether there is significant autocorrelation at any of the first 10 lags 
#> of the residuals. If the p-values are above the significance level (usually 0.05), 
#> the residuals do not have significant autocorrelation.

# Check for serial correlation in residuals
serial_test <- serial.test(var_model, lags.pt = optimal_lag, type = "PT.adjusted") # TODO: "Portmanteau" ?
print(serial_test)

# Plotting residuals
plot(residuals(var_model)) # TODO: implement in ggplot for all vars

# Stability of the VAR model
stability(var_model)

# Check for autocorrelation visually
par(mfrow = c(2, 2))
acf(residuals(var_model)[, "ma_pct_pageviews"], main = "ACF of Pageviews Residuals")
acf(residuals(var_model)[, "ma_cases"], main = "ACF of Cases Residuals")
acf(residuals(var_model)[, "ma_n_articles"], main = "ACF of Articles Residuals")
acf(residuals(var_model)[, "ma_n_studies"], main = "ACF of Studies Residuals")

# Ljung-Box Test
Box.test(residuals(var_model)[, "ma_pct_pageviews"], lag = 20, type = "Ljung-Box")
Box.test(residuals(var_model)[, "ma_cases"], lag = 20, type = "Ljung-Box")
Box.test(residuals(var_model)[, "ma_n_articles"], lag = 20, type = "Ljung-Box")
Box.test(residuals(var_model)[, "ma_n_studies"], lag = 20, type = "Ljung-Box")

# Normality test on residuals
normality_test <- normality.test(var_model)
print(normality_test)

# Checking model robustness by slightly varying around the optimal lag
lags_to_test <- c(optimal_lag - 1, optimal_lag, optimal_lag + 1)
robustness_results <- lapply(lags_to_test, function(lag) {
  model <- VAR(ts_df, p = lag, type = "both")
  summary(model)
})

# Display robustness results
print(robustness_results)


# Forecasting ==================================================================
n_ahead <- 10  # Number of periods to forecast ahead
forecast_results <- predict(var_model, n.ahead = n_ahead)

# Plot the forecast
plot(forecast_results)


# Impulse Response Analysis ====================================================
#> An impulse response function (IRF) can help to understand how a shock to one 
#> variable affects the others in the VAR system over time.

# Impulse response analysis
irf.results <- irf(var_model, n.ahead = n_ahead, boot = TRUE)

# Plot IRF
plot(irf.results)


# Granger causality ============================================================
#> Apply the Granger causality test within the VAR framework to assess whether Wikipedia traffic volumes can be considered a predictor of mpox case trajectories.

# Test if pageviews Granger-cause mpox cases
granger_pageviews <- causality(var_model, cause = "pageviews_diff1")
granger_pageviews

# Test if mpox cases Granger-cause pageviews
granger_cases <- causality(var_model, cause = "cases_diff1")
granger_cases

#> The key aspect of the result to look at is the p-value. If the p-value is less than your significance level (commonly set at 0.05), you can conclude that there is evidence to suggest that the cause variable Granger-causes the other variable(s) in the VAR model


# Validate forecast ============================================================
# Goodness of fit metrics

# Expanding window validation

# OR split data into train/test and apply forecast to new data