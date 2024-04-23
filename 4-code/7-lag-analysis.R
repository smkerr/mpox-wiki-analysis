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
  left_join(mpox_df, by = join_by(page_title, date)) |> # add back existing data 
  group_by(page_title) |> 
  fill(country, iso2, iso3, project, wikidata_id, page_id, .direction = "updown") |> # fill missing info
  ungroup() |> 
  select(-cases) |> 
  left_join(cases_by_date, by = join_by(date)) |> # add back case data
  select(-pageviews_ceil) |> 
  left_join(total_pageviews_by_date, join_by(date)) |> # add monthly total pageview data 
  select(country, iso2, iso3, project, wikidata_id, page_id, page_title, date, cases, pct_pageviews, n_articles, n_studies) 
# TODO: Add 7-day rolling avg for cases
# TODO: Add 7-day rolling avg for pct_pageviews


# Function to shift pageviews and build the model ==============================
lag_and_model <- function(data, lag) {
  # Order chronologically
  data <- data |> 
    arrange(date)
  
  # Independent variables
  independent_vars <- names(data)[!names(data) %in% c("country", "iso2", "iso3", "project", "cases", "date", "pageviews", "pageviews_ceil")]
  independent_vars_quoted <- sprintf("`%s`", independent_vars) # add backticks to account for irregular characters in variable names
  formula_str <- paste("cases ~", paste(independent_vars_quoted, collapse = " + "))
  model_formula <- as.formula(formula_str)
  
  # Shift the lagged variables
  if (lag > 0) {
    shifted_df <- data |> 
      mutate(across(all_of(independent_vars), ~lead(.x, abs(lag)))) |> 
      drop_na()
  } else if (lag < 0) {
    shifted_df <- data |> 
      mutate(across(all_of(independent_vars), ~lag(.x, abs(lag)))) |> 
      drop_na() # TODO: Should NAs be dropped???
  } else { # lag == 0
    shifted_df <- data
  }
  
  # Build the model
  model <- lm(model_formula, data = shifted_df) # TODO: Spearman correlation????

  return(model)
}


# Prepare data for lagged modeling =============================================
# Model 1: "Mpox" pageviews
lag_df1 <- lag_df |> 
  filter(page_title == "Mpox") |> 
  select(-wikidata_id, -page_id, -n_articles, -n_studies) |> 
  pivot_wider(names_from = page_title, values_from = pct_pageviews) |> 
  arrange(date)

# Model 2: Included articles pageviews
lag_df2 <- lag_df |> 
  select(-wikidata_id, -page_id, -n_articles, -n_studies) |> 
  pivot_wider(names_from = page_title, values_from = pct_pageviews) |> 
  arrange(date)

# Model 3: "Mpox" pageviews + news + academia
lag_df3 <- lag_df |> 
  filter(page_title == "Mpox") |> 
  select(-wikidata_id, -page_id) |> 
  pivot_wider(names_from = page_title, values_from = pct_pageviews) |> 
  arrange(date)

# Model 4: Included articles pageviews + news + academia
lag_df4 <- lag_df |> 
  select(-wikidata_id, -page_id,) |> 
  pivot_wider(names_from = page_title, values_from = pct_pageviews) |> 
  arrange(date)

# Fit models to each lag =======================================================
# Define range of lags
lags <- -28:28 # TODO: Should align with findings from segmented regression

# Model 1: "Mpox" pageviews
models1 <- map(lags, ~ lag_and_model(lag_df1, .x)) |> setNames(lags)
model_results1 <- map_df(models1, glance) |> 
  mutate(lag = lags) |> 
  relocate(lag, .before = everything())
model_coeffs1 <- map_df(models1, tidy) |> 
  arrange(term) |> 
  mutate(lag = rep(lags, 2)) |> 
  relocate(lag, .before = everything()) |> 
  arrange(lag, term)

# # Model 2: Included articles pageviews
# models2 <- map(lags, ~ lag_and_model(lag_df2, .x)) |> setNames(lags)
# model_results2 <- map_df(models2, glance) |> 
#   mutate(lag = lags) |> 
#   relocate(lag, .before = everything())
# model_coeffs2 <- map_df(models2, tidy) |> 
#   arrange(term) |> 
#   mutate(lag = rep(lags, 2)) |> 
#   relocate(lag, .before = everything()) |> 
#   arrange(lag, term)

# Model 3: "Mpox" pageviews + news + academia
models3 <- map(lags, ~ lag_and_model(lag_df3, .x)) |> setNames(lags)
model_results3 <- map_df(models3, glance) |> 
  mutate(lag = lags) |> 
  relocate(lag, .before = everything())
model_coeffs3 <- map_df(models3, tidy) |> 
  arrange(term) |> 
  mutate(lag = rep(lags, 4)) |> 
  relocate(lag, .before = everything()) |> 
  arrange(lag, term)


# # Model 4: Included articles pageviews + news + academia
# models4 <- map(lags, ~ lag_and_model(lag_df4, .x)) |> setNames(lags)
# model_results4 <- map_df(models4, glance) |> 
#   mutate(lag = lags) |> 
#   relocate(lag, .before = everything())
# model_coeffs4 <- map_df(models4, tidy) |> 
#   arrange(term) |> 
#   mutate(lag = rep(lags, 2)) |> 
#   relocate(lag, .before = everything()) |> 
#   arrange(lag, term)

# Evaluate best fit ============================================================

# TODO: The goal here is to obtain the max lag to be used in VAR model

# Model 1: "Mpox" pageviews
model_results1 |> slice_min(AIC)
model_results1 |> slice_min(BIC)
model_results1 |> slice_max(r.squared)
model_results1 |> slice_max(adj.r.squared)

# # Model 2: Included articles pageviews
# model_results2 |> slice_min(AIC)
# model_results2 |> slice_min(BIC)
# model_results2 |> slice_max(r.squared)
# model_results2 |> slice_max(adj.r.squared)

# Model 3: "Mpox" pageviews + news + academia
model_results3 |> slice_min(AIC)
model_results3 |> slice_min(BIC)
model_results3 |> slice_max(r.squared)
model_results3 |> slice_max(adj.r.squared)

# # Model 4: Included articles pageviews + news + academia
# model_results4 |> slice_min(AIC)
# model_results4 |> slice_min(BIC)
# model_results4 |> slice_max(r.squared)
# model_results4 |> slice_max(adj.r.squared)


# TODO: Why so few obs???

# Model 1: "Mpox" pageviews
# Model 2: Included articles pageviews
# Model 3: "Mpox" pageviews + news + academia
# Model 4: Included articles pageviews + news + academia

# TODO: Figure out better way to visualize p-values (re-introduce heat maps?)
# Viz check
model_results1 |> 
  ggplot(aes(lag, r.squared)) + 
  geom_line() + 
  geom_smooth() +
  geom_vline(xintercept = model_results1 |> slice_max(r.squared) |> pull(lag), color = "red", linetype = "dashed") +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()

# Viz check
model_results1 |> 
  ggplot(aes(lag, adj.r.squared)) + 
  geom_line() + 
  geom_smooth() +
  geom_vline(xintercept = model_results1 |> slice_max(adj.r.squared) |> pull(lag), color = "red", linetype = "dashed") +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()

# Viz check
model_results1 |> 
  ggplot(aes(lag, AIC)) + 
  geom_line() + 
  geom_smooth() +
  geom_vline(xintercept = model_results1 |> slice_min(AIC) |> pull(lag), color = "red", linetype = "dashed") +
  scale_x_continuous(n.breaks = 10) +
  theme_minimal()

# Viz check 
model_coeffs1 |> 
  ggplot(aes(lag, estimate, color = term)) + 
  geom_line() + 
  geom_smooth() +
  scale_y_continuous(labels = label_comma()) + 
  theme_minimal()

# Viz check 
model_coeffs1 |> 
  ggplot(aes(lag, p.value, color = term)) + 
  #geom_line() + 
  geom_smooth(se = FALSE) +
  geom_hline(yintercept = 0.05, color = "red", linetype = "dashed") + 
  scale_y_continuous(limits = c(0, 1)) + 
  theme_minimal()

# Viz check
model_results3 |> 
  ggplot(aes(lag, r.squared)) + 
  geom_line() + 
  geom_smooth() +
  geom_vline(xintercept = model_results3 |> slice_max(r.squared) |> pull(lag), color = "red", linetype = "dashed") +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()

# Viz check
model_results3 |> 
  ggplot(aes(lag, adj.r.squared)) + 
  geom_line() + 
  geom_smooth() +
  geom_vline(xintercept = model_results3 |> slice_max(adj.r.squared) |> pull(lag), color = "red", linetype = "dashed") +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()

# Viz check
model_results3 |> 
  ggplot(aes(lag, AIC)) + 
  geom_line() + 
  geom_smooth() +
  geom_vline(xintercept = model_results3 |> slice_min(AIC) |> pull(lag), color = "red", linetype = "dashed") +
  scale_x_continuous(n.breaks = 10) +
  theme_minimal()

# Viz check 
model_coeffs3 |> 
  ggplot(aes(lag, estimate, color = term)) + 
  geom_line() + 
  geom_smooth() +
  scale_y_continuous(labels = label_comma()) + 
  theme_minimal()

# Viz check 
model_coeffs3 |> 
  ggplot(aes(lag, p.value, color = term)) + 
  #geom_line() + 
  geom_smooth(se = FALSE) +
  geom_hline(yintercept = 0.05, color = "red", linetype = "dashed") + 
  scale_y_continuous(limits = c(0, 1)) + 
  theme_minimal()


# TODO: This may have flipped????
# correlations will contain the Spearman correlation coefficients for each lag
## negative lags correspond with the time-lag effect of new cases on pageviews
## positive lags correspond with the time-lag effect of pageviews on new cases 


# Get predictions
augment(models1[[1]]) |>
  ggplot() + 
  geom_line(aes(x = lag, y = cases), color = ("blue")) + 
  geom_line(aes(x = lag, y = `.fitted`), color = ("red")) + 
  theme_minimal()

# residuals
augment(models[[26]]) |>
  ggplot() + 
  #geom_point(aes(x = `.rownames`, y = cases), color = ("blue"), size = 2) + 
  geom_col(aes(x = `.rownames`, y = `.resid`), fill = ("red"), linewidth = 2) + 
  theme_minimal()


# Store results
write_csv(model_results, here(glue("3-data/output/lag-analysis-results.csv")))

# TODO: Move visualizations to figures script

# Expanding window validation ==================================================
# Key parameters
initial_size <- 7  # initial window size of 7 days
horizon <- 7       # forecast 7 days ahead
step <- 1          # move one day at a time

plot_list <- list()
results <- list()

for (i in seq_len(nrow(lag_pivoted) - initial_size - horizon + 1)) {
  train_end <- i + initial_size - 1 + i - 1  # Expanding window
  if (train_end + horizon > nrow(lag_pivoted)) break
  
  # Prepare training data 
  # Key parameters
initial_size <- 7  # initial window size of 7 days
horizon <- 7       # forecast 7 days ahead
step <- 1          # move one day at a time

plot_list <- list()
results <- list()

for (i in seq_len(nrow(lag_pivoted) - initial_size - horizon + 1)) {
  train_end <- i + initial_size - 1 + i - 1  # Expanding window
  if (train_end + horizon > nrow(lag_pivoted)) break
  
  # Prepare the training data
  train_data <- lag_pivoted[1:train_end, ]
  
  # Identify independent variables, excluding specific columns
  independent_vars <- names(train_data)[!names(train_data) %in% c("country", "iso2", "iso3", "cases", "date", "project")]
  independent_vars_quoted <- sprintf("`%s`", independent_vars) # add backticks to account for irregular characters in variable names
  formula_str <- paste("cases ~", paste(independent_vars_quoted, collapse = " + "))
  model_formula <- as.formula(formula_str)
  
  # Fit the model using the formula
  model <- lm(model_formula, data = train_data)
  
  # Prepare the test data and forecast
  test_data <- lag_pivoted[(train_end + 1):(train_end + horizon), ]
  forecast <- predict(model, newdata = test_data)
  
  # Combine forecast and actual data for plotting
  forecast_dates <- lag_pivoted$date[(train_end + 1):(train_end + horizon)]
  forecast_df <- data.frame(Date = forecast_dates, Forecast = forecast)
  actual_df <- data.frame(Date = forecast_dates, Actual = test_data$cases)
  plot_df <- left_join(forecast_df, actual_df, by = "Date")
  
  
  
  
  
  
  
  
  
  
  train_data <- window(ts(lag_pivoted$cases, frequency = 1), start = 1, end = train_end)
  model <- auto.arima(train_data)
  
  # Forecast
  test_end <- train_end + horizon
  test_data <- window(ts(lag_pivoted$cases, frequency = 1), start = train_end + 1, end = test_end)
  forecast <- forecast(model, h = horizon)
  
  # Plot forecast vs actual
  forecast_dates <- seq.Date(lag_pivoted$date[train_end + 1], by = "day", length.out = horizon)
  forecast_df <- data.frame(Date = forecast_dates, Forecast = forecast$mean)
  actual_df <- data.frame(Date = forecast_dates, Actual = as.numeric(test_data))
  plot_df <- left_join(forecast_df, actual_df, by = "Date")
  
  p <- ggplot(plot_df, aes(x = Date)) +
    geom_line(aes(y = Forecast, colour = "Forecast")) +
    geom_line(aes(y = Actual, colour = "Actual")) +
    labs(title = paste("Forecast vs Actual from Day", train_end + 1),
         y = "Cases",
         colour = "Legend") +
    theme_minimal()
  
  plot_list[[i]] <- p  # Store plot in list
  
  # Store results
  results[[i]] <- accuracy(forecast, test_data)
}

# Calculate mean error across all forecasts
mean_error <- mean(map_dbl(results, function(x) x["Test set", "ME"]))
print(mean_error)

  train_data <- window(ts(lag_pivoted$cases, frequency = 1), start = 1, end = train_end)
  model <- auto.arima(train_data)
  
  # Forecast
  test_end <- train_end + horizon
  test_data <- window(ts(lag_pivoted$cases, frequency = 1), start = train_end + 1, end = test_end)
  forecast <- forecast(model, h = horizon)
  
  # Plot forecast vs actual
  forecast_dates <- seq.Date(lag_pivoted$date[train_end + 1], by = "day", length.out = horizon)
  forecast_df <- data.frame(Date = forecast_dates, Forecast = forecast$mean)
  actual_df <- data.frame(Date = forecast_dates, Actual = as.numeric(test_data))
  plot_df <- left_join(forecast_df, actual_df, by = "Date")
  
  p <- ggplot(plot_df, aes(x = Date)) +
    geom_line(aes(y = Forecast, colour = "Forecast")) +
    geom_line(aes(y = Actual, colour = "Actual")) +
    labs(title = paste("Forecast vs Actual from Day", train_end + 1),
         y = "Cases",
         colour = "Legend") +
    theme_minimal()
  
  plot_list[[i]] <- p  # Store plot in list
  
  # Store results
  results[[i]] <- accuracy(forecast, test_data)
}

# Calculate mean error across all forecasts
mean_error <- mean(map_dbl(results, function(x) x["Test set", "ME"]))
print(mean_error)

results
plot_list[65]
