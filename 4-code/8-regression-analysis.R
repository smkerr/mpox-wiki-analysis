# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================


# Setup ========================================================================
# Load data 
mpox_df <- read_csv(here("3-data/output/mpox-data.csv"))


# Prepare data =================================================================
mpox_ts <- mpox_df |> 
  filter(page_title == "Mpox") |> 
  complete(date = seq.Date(from = min(as_date("2022-05-10")), to = max(mpox_df$date), by = 1)) |> # fill in missing dates
  filter(date >= as_date("2022-05-10")) |> 
  filter(date <= as_date("2022-05-10") + days(180)) |> 
  complete(fill = list(cases = 0, pageviews = 450, n_articles = 0, n_studies = 0)) |> # impute NAs (set missing pageviews equal to threshold)
  fill(country, iso2, iso3, project, wikidata_id, page_id, page_title, pageviews_ceil, .direction = "downup") |> 
  # calculate rolling averages to smooth values
  mutate(
    pct_pageviews = pageviews / pageviews_ceil,
    roll_pct_pageviews = slide_dbl(pct_pageviews, ~mean(.x, na.rm = TRUE), .before = 6), 
    roll_cases = slide_dbl(cases, .f = ~mean(.x, na.rm = TRUE), .before = 6),
    roll_n_articles = slide_dbl(n_articles, .f = ~mean(.x, na.rm = TRUE), .before = 6),
    roll_n_studies = slide_dbl(n_studies, .f = ~mean(.x, na.rm = TRUE), .before = 6)
  ) |> 
  select(-cases, -n_articles, -n_studies, -pageviews, -pageviews_ceil, -pct_pageviews)

# Independent variables
independent_vars <- c("roll_pct_pageviews") #, "roll_n_articles", "roll_n_studies")
formula_str <- paste("roll_cases ~", paste(independent_vars, collapse = " + ")) # TODO: Log cases?
model_formula <- as.formula(roll_cases ~ roll_pct_pageviews + roll_n_articles + roll_n_studies)
  
# Build the model
model <- lm(model_formula, data = mpox_ts) # TODO: Spearman correlation????
summary(model)
model |> tidy()
model |> glance()
model |> augment() |> 
  mutate(time = row_number()) |> 
  ggplot() + 
  geom_line(aes(x = time, y = .fitted), color = "darkgrey", linetype = "longdash") + 
  geom_line(aes(x = time, y = roll_cases), color = "black") + 
  theme_minimal()
  
# Spearman correlation
cor.test(mpox_ts$roll_cases, mpox_ts$roll_pct_pageviews, method = "spearman")
cor.test(mpox_ts$roll_cases, mpox_ts$roll_n_articles, method = "spearman")
cor.test(mpox_ts$roll_cases, mpox_ts$roll_n_studies, method = "spearman")

# Viz check
ggplot(mpox_ts, aes(x = roll_n_studies, y = log(roll_cases))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  theme_minimal()

# Plot diagnostics
par(mfrow = c(2, 2))
plot(model)




# Validate forecast ============================================================
# Goodness of fit metrics

# Expanding window validation

# OR split data into train/test and apply forecast to new data










# Get predictions
augment(models1[[1]]) |>
  ggplot() + 
  geom_line(aes(x = lag, y = roll_cases), color = ("blue")) + 
  geom_line(aes(x = lag, y = .fitted), color = ("red")) + 
  theme_minimal()

# residuals
augment(models[[26]]) |>
  ggplot() + 
  #geom_point(aes(x = `.rownames`, y = cases), color = ("blue"), size = 2) + 
  geom_col(aes(x = `.rownames`, y = `.resid`), fill = ("red"), linewidth = 2) + 
  theme_minimal()









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









# Assuming mpox_ts is your final dataset used for modeling
n <- nrow(mpox_ts)  # Total number of observations

expanding_window_validation <- function(start_size, data) {
  results <- tibble(
    index = integer(),
    actual = numeric(),
    predicted = numeric()
  )
  
  for (i in seq(start_size, n)) {
    train_data <- data[1:i, ]
    test_data <- data[(i+1):min(i+1, n), ]
    
    if (nrow(test_data) == 0) break  # Stop if there's no more data to predict
    
    model <- lm(roll_cases ~ roll_pct_pageviews + roll_n_articles + roll_n_studies, data = train_data)
    prediction <- predict(model, newdata = test_data)
    
    results <- results %>% add_row(
      index = i + 1,
      actual = test_data$roll_cases,
      predicted = prediction
    )
  }
  
  results
}

# Adjust the 'start_size' parameter as needed based on your specific context
validation_results <- expanding_window_validation(start_size = 30, data = mpox_ts)  # Starting with 30 days

library(Metrics)
mae_value <- mae(validation_results$actual, validation_results$predicted)
rmse_value <- rmse(validation_results$actual, validation_results$predicted)

print(paste("MAE:", mae_value))
print(paste("RMSE:", rmse_value))

ggplot(validation_results, aes(x = index)) +
  geom_line(aes(y = actual, colour = "Actual"), size = 1) +
  geom_line(aes(y = predicted, colour = "Predicted"), size = 1, linetype = "dashed") +
  labs(title = "Expanding Window Validation: Predicted vs Actual Cases",
       colour = "Legend") +
  theme_minimal()

