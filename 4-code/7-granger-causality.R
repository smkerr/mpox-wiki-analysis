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
  ggnewscale, ###
  ggplot2,
  here,
  lubridate, 
  stringr,
  tibble, 
  tseries, 
  purrr, 
  readr, 
  scales, 
  slider, 
  tidyr, 
  tseries, ###
  vars,
  install = FALSE
  )

# Load data 
mpox_df <- read_csv(here("3-data/output/mpox-data.csv")) |> 
  filter(date >= as_date("2022-05-10") & date <= as_date("2023-02-05")) 

# Prepare data =================================================================
df <- left_join(
  # calculate 7-day rolling averages for pageviews 
  mpox_df |> 
    filter(page_title == "Mpox") |> 
    group_by(country, iso2, iso3, project, wikidata_id, page_id, page_title) |> 
    mutate(
      pageviews = ifelse(is.na(pageviews), 450, pageviews), # set missing values equal to inclusion threshold
      pct_pageviews = pageviews / pageviews_ceil, # normalize data as share of monthly pageviews
      roll_pct_pageviews = slide_dbl(pct_pageviews, ~mean(.x, na.rm = TRUE), .before = 6)
    ) |> 
    ungroup() |> 
    select(-cases:-n_studies),
  # calculate 7-day rolling averages for cases, news articles, & scientific studies
  mpox_df |> 
    distinct(date, cases, n_articles, n_studies) |> 
    mutate(roll_cases = slide_dbl(cases, .f = ~mean(.x, na.rm = TRUE), .before = 6)) |> 
    select(date, starts_with("roll")),
  by = join_by(date)
)

# Save data
write_csv(df, here("3-data/output/granger-causality/granger-causality-data.csv"))

# Viz check
df |> 
  pivot_longer(cols = starts_with("roll"), names_to = "var", values_to = "value") |>
  ggplot(aes(x = date, y = value, color = var)) +
  geom_line() +
  facet_wrap(~var, ncol = 1, scales = "free_y") +
  scale_x_date(date_labels = "%b\n%Y") +
  scale_y_continuous(labels = label_number()) +
  labs(title = "Time Series Plot", x = NULL, y = "Value") +
  theme_minimal() + 
  theme(legend.position = "none") 


# Test for stationarity ========================================================
#> Implement ADF test to check for stationarity in all included variables.
#> Non-stationary data can lead to spurious results in subsequent analyses.
adf_results1 <- df |> 
  select(starts_with("roll")) |> 
  map(adf.test, alternative = "stationary")
adf_results1 <- adf_results1 |> 
  map_dfr(tidy) |> 
  mutate(var = names(adf_results1)) |> 
  relocate(var, .before = everything())
non_stationary1 <- adf_results1 |> 
  filter(p.value > 0.05)
print(non_stationary1)

# First-order differencing to achieve stationarity
if (nrow(non_stationary1) > 0) {
  # drop first observation
  df1 <- df[-1, ]
  
  # apply first-order differencing to non-stationary vars
  for (var in non_stationary1$var) {
    df1[[var]] <- diff(df[[var]], differences = 1)
  }
}

# Viz check
df1 |> 
  pivot_longer(cols = starts_with("roll"), names_to = "var", values_to = "value") |> 
  ggplot(aes(x = date, y = value, color = var)) +
  geom_line() +
  facet_wrap(~var, ncol = 1, scales = "free_y") +
  scale_x_date(date_labels = "%b\n%Y") +
  labs(title = "Time Series Plot", x = NULL, y = "Value") +
  theme_minimal()

# Check for stationarity 
adf_results2 <- df1 |> 
  select(starts_with("roll")) |> 
  map(adf.test, alternative = "stationary")
adf_results2 <- adf_results2[1:2] |> 
  map_dfr(tidy) |> 
  mutate(var = names(adf_results2[1:2])) |> 
  relocate(var, .before = everything())
non_stationary2 <- adf_results2 |> 
  filter(p.value > 0.05)
# The following have a p-value >0.05:
print(non_stationary2)
#> p-values of ADF test for all variables are <0.05, therefore we can consider
#> each variable to be stationary


# Determine optimal lag length =================================================
# Determine the optimal number of lags based on information criteria (e.g., AIC)
lag_selection <- df1 |> 
  select(starts_with("roll")) |> 
  VARselect(lag.max = 37, type = "none") 
optimal_lag <- lag_selection$selection["AIC(n)"]


# Convert to a time-series object ==============================================
mpox_ts <- df1 |> 
  select(starts_with("roll")) |> 
  ts()


# Fit VAR model ================================================================
var_model <- VAR(mpox_ts, p = optimal_lag, type = "none")
var_model

# Summary of VAR model
summary(var_model)


# Validate the model ===========================================================
# Check for autocorrelation in residuals
#> Check whether there is significant autocorrelation at any of the first 10 lags 
#> of the residuals. If the p-values are above the significance level (usually 0.05), 
#> the residuals do not have significant autocorrelation.

# Plotting residuals
plot(residuals(var_model)) 

# Stability of the VAR model
stability(var_model)

# Check for autocorrelation visually
par(mfrow = c(1, 2))
acf(residuals(var_model)[, "roll_pct_pageviews"], main = "ACF of Pageviews Residuals")
acf(residuals(var_model)[, "roll_cases"], main = "ACF of Cases Residuals")

# Ljung-Box Test
Box.test(residuals(var_model)[, "roll_pct_pageviews"], lag = optimal_lag, type = "Ljung-Box")
Box.test(residuals(var_model)[, "roll_cases"], lag = optimal_lag, type = "Ljung-Box")

# Normality test on residuals
normality.test(var_model)

# Checking model robustness by slightly varying around the optimal lag
lags_to_test <- c(optimal_lag - 1, optimal_lag, optimal_lag + 1)
robustness_results <- lapply(lags_to_test, function(lag) {
  model <- VAR(mpox_ts, p = lag, type = "both")
  summary(model)
})
robustness_results

# Check for multicollinearity
cor(var_model$datamat) 


# Impulse Response Analysis ====================================================
#> An impulse response function (IRF) can help to understand how a shock to one 
#> variable affects the others in the VAR system over time.
n_ahead = 10
irf_results <- irf(var_model, cumulative = FALSE, n.ahead = n_ahead, boot = TRUE, runs = 100)

# Extracting and preparing data
irf_df <- irf_results$irf |>
  as.data.frame() |>
  rownames_to_column(var = "Time") |>
  pivot_longer(-Time, names_to = "Series", values_to = "Value")

# Prepare the upper and lower bounds for confidence intervals
upper_df <- irf_results$Upper |>
  as.data.frame() |>
  rownames_to_column(var = "Time") |>
  pivot_longer(-Time, names_to = "Series", values_to = "Upper")

lower_df <- irf_results$Lower |>
  as.data.frame() |>
  rownames_to_column(var = "Time") |>
  pivot_longer(-Time, names_to = "Series", values_to = "Lower")

# Join data frames
irf_df <- irf_df |>
  left_join(upper_df, by = c("Time", "Series")) |>
  left_join(lower_df, by = c("Time", "Series")) |> 
  mutate(Time = as.numeric(Time)) |> 
  mutate(Impulse = str_extract(Series, "^[^.]+"), Response = str_extract(Series, "(?<=\\.)[^.]*$"))
  
# Plotting IRF
ggplot(data = irf_df, aes(x = Time, y = Value, color = Series, fill = Series)) +
  geom_hline(yintercept = 0, color = muted("red"), linetype = "dashed") + 
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2) +  # Confidence interval
  geom_line() +  # IRF lines
  facet_wrap(~Series, scales = "free_y") +
  labs(title = "Impulse Response Function",
       x = "Time (Lags)",
       y = "Response",
       color = NULL,
       fill = "Response to"
       ) +
  theme_bw() +
  theme(legend.position = "none")


# Granger causality ============================================================
#> Apply the Granger causality test within the VAR framework
var_model$datamat |> 
  names() |> 
  str_extract("^roll_[^\\.]+$") |> 
  na.omit() |> 
  as.vector() |> 
  map(~causality(var_model, cause = .x))
  
granger_pageviews <- causality(var_model, cause = "roll_pct_pageviews")
granger_cases <- causality(var_model, cause = "roll_cases")

granger_pageviews
granger_cases

#> The key aspect of the result to look at is the p-value. If the p-value is less than your significance level (commonly set at 0.05), you can conclude that there is evidence to suggest that the cause variable Granger-causes the other variable(s) in the VAR model
