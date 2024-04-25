# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Author: Steve Kerr
# Date: April 2024
# ==============================================================================


# Setup ========================================================================
# Load packages
pacman::p_load(MASS, broom, dplyr, ggplot2, glue, here, lubridate, purrr, readr, slider, tidyr)

# Load data 
#mpox_df <- read_csv(here("3-data/output/mpox-data.csv"))
mpox_train <- read_csv(here("3-data/output/mpox-data.csv")) |> 
  filter(date >= as_date("2022-05-10") & date <= as_date("2023-02-05")) 
mpox_test <- read_csv(here("3-data/output/mpox-data.csv")) |> 
  filter(date >= as_date("2023-02-06") & date <= as_date("2024-02-27")) 
load(here("3-data/output/mpox-pages-included.RData"))

# Check model fit summary for expanding window validation 
# Opt: Apply to period outside study period


# Prepare data =================================================================
mpox_df <- left_join(
  # calculate 7-day rolling averages for pageviews 
  mpox_train |> 
    filter(page_title %in% included_articles) |> 
    # Expand to include missing dates
    complete(
      page_title = included_articles,
      date = seq.Date(from = min(mpox_train$date, na.rm = TRUE), to = max(mpox_train$date, na.rm = TRUE), by = 1),
    ) |> 
    # Fill in missing info
    fill(country, iso2, iso3, project, wikidata_id, page_id, page_title, pageviews_ceil, .direction = "downup") |> 
    mutate(
      pageviews = ifelse(is.na(pageviews) | pageviews == 0, 450, pageviews), ###
      pct_pageviews = pageviews / pageviews_ceil,
    ) |> 
    group_by(country, iso2, iso3, project, wikidata_id, page_id, page_title) |> 
    mutate(roll_pct_pageviews = slide_dbl(pct_pageviews, ~mean(.x, na.rm = TRUE), .before = 6)) |>
    ungroup() |> 
    mutate(across(everything(), ~replace(., is.nan(.), NA))) |>
    select(-cases:-pageviews_ceil, -n_articles, -n_studies),
  # calculate 7-day rolling averages for cases, news articles, & scientific studies
  mpox_train |> 
    distinct(date, cases, n_articles, n_studies) |> 
    mutate(
      roll_cases = slide_dbl(cases, .f = ~mean(.x, na.rm = TRUE), .before = 6),
      roll_n_articles = slide_dbl(n_articles, .f = ~mean(.x, na.rm = TRUE), .before = 6),
      roll_n_studies = slide_dbl(n_studies, .f = ~mean(.x, na.rm = TRUE), .before = 6)
    ) |> 
    select(date, starts_with("roll")),
  by = join_by(date)
  ) |> 
  select(-wikidata_id, -page_id) |> 
  pivot_wider(names_from = page_title, values_from = roll_pct_pageviews) 

# Define ID variables
id_vars <- c("country", "iso2", "iso3", "project")


# Prepare Data for Modeling ====================================================
model_datasets <- list(
  # Model 1: "Mpox" pageviews
  "mpox" = mpox_df |> 
    select(all_of(id_vars), date, roll_cases, Mpox),
  # Model 2: Included articles pageviews
  "included_articles" = mpox_df |> 
    select(all_of(id_vars), date, roll_cases, all_of(included_articles)),
  # Model 3: "Mpox" pageviews + news + academia
  "mpox_covars" = mpox_df |> 
    select(all_of(id_vars), date, starts_with("roll"), Mpox),
  # Model 4: Included articles pageviews + news + academia
  "included_articles_covars" = mpox_df |> 
    select(all_of(id_vars), date, starts_with("roll"), all_of(included_articles))
)


# Model Specification ==========================================================
formulas <- list(
  # Model 1: "Mpox" pageviews
  mpox = as.formula("roll_cases ~ Mpox"),
  # Model 2: Included articles pageviews
  included_articles = as.formula(glue("roll_cases ~ {paste(sprintf('`%s`', included_articles), collapse = '+')}")),
  # Model 3: "Mpox" pageviews + news + academia
  mpox_covars = as.formula("roll_cases ~ Mpox + roll_n_articles + roll_n_studies"),
  # Model 4: Included articles pageviews + news + academia
  included_articles_covars = as.formula(glue("roll_cases ~ {paste(sprintf('`%s`', included_articles), collapse = '+')} + roll_n_articles + roll_n_studies"))
)


# Fit Models ===================================================================
models <- map2(formulas, model_datasets, ~lm(.x, .y))


# Summarize Models =============================================================
# Combine summaries into single dataframe
model_summaries <- map(models, glance)
model_summary_table <- bind_rows(model_summaries, .id = "Model")

# Combine coefficient estimates into single dataframe
coef_summaries <- map(models, tidy)
coef_summary_table <- bind_rows(model_summaries, .id = "Model")

# TODO: Save summary tables
# write.csv(model_summary_table, "model_summaries.csv")
# write.csv(coef_summary_table, "coef_summaries.csv")


# Model Diagnostics ============================================================
check_diagnostics <- function(model) {
  par(mfrow = c(2,2))
  plot(model)  # plots Residuals vs Fitted, Normal Q-Q, Scale-Location, Residuals vs Leverage
}

# Visualize diagnostic checks 
walk(models, check_diagnostics)


# Plot Model Fit ===============================================================
plot_model <- function(formula, data) {
  # Fit the model
  model <- lm(formula, data)
  
  # Augment the data with the model's fitted values and residuals
  augmented_data <- augment(model) |> 
    mutate(date = data$date)
  
  # Plotting
  p <- ggplot(augmented_data, aes(x = date)) +
    geom_line(aes(y = roll_cases), color = "black") +
    geom_line(aes(y = .fitted), color = "red", linetype = "longdash") +
    theme_minimal() +
    labs(
      title = "Actual vs Fitted Cases",
      y = "Cases",
      x = "Date"
      )
  
  return(p)
}

# Plot actual vs fitted cases
plot_model(formulas$mpox, model_datasets$mpox)
plot_model(formulas$included_articles, model_datasets$included_articles)
plot_model(formulas$mpox_covars, model_datasets$mpox_covars)
plot_model(formulas$included_articles_covars, model_datasets$included_articles_covars)


# Check Residuals ==============================================================
plot_model <- function(formula, data) {
  # Fit the model
  model <- lm(formula, data)
  
  # Augment the data with the model's fitted values and residuals
  augmented_data <- augment(model)
  
  # Plotting
  p <- ggplot(augmented_data, aes(x = date)) +
    geom_line(aes(y = roll_cases), color = "black") +
    geom_line(aes(y = .resid), color = "red", linetype = "longdash") +
    theme_minimal() +
    labs(
      title = "Actual vs Fitted Cases",
      y = "Cases",
      x = "Date"
    )
  
  return(p)
}

lm(formulas$mpox, model_datasets$mpox) |> augment() |> 
  ggplot(aes(x = date)) +
  geom_line(aes(y = .fitted), color = "black") +
  geom_line(aes(y = .resid), color = "red", linetype = "longdash") +
  theme_minimal() +
  labs(
    title = "Actual vs Fitted Cases",
    y = "Cases",
    x = "Date"
  )

model <- lm(formulas$mpox, model_datasets$mpox)
plot(model, which = 1)  # Residuals vs Fitted

qqnorm(resid(model))
qqline(resid(model), col = "steelblue")

plot(model, which = 3)

hist(resid(model))

acf(resid(model))

# Residuals vs Fitted with ggplot2
model |> 
  augment() |> 
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals")


# Validate Forecast ============================================================
## Prepare data ----------------------------------------------------------------
mpox_test <- left_join(
  # calculate 7-day rolling averages for pageviews 
  mpox_test |> 
    filter(page_title %in% included_articles) |> 
    # Expand to include missing dates
    complete(
      page_title = included_articles,
      date = seq.Date(from = min(mpox_test$date, na.rm = TRUE), to = max(mpox_test$date, na.rm = TRUE), by = 1),
    ) |> 
    # Fill in missing info
    fill(country, iso2, iso3, project, wikidata_id, page_id, page_title, pageviews_ceil, .direction = "downup") |> 
    mutate(
      pageviews = ifelse(is.na(pageviews) | pageviews == 0, 90, pageviews), ###
      pct_pageviews = pageviews / pageviews_ceil,
    ) |>
    group_by(country, iso2, iso3, project, wikidata_id, page_id, page_title) |> 
    mutate(roll_pct_pageviews = slide_dbl(pct_pageviews, ~mean(.x, na.rm = TRUE), .before = 6)) |>
    ungroup() |> 
    mutate(across(everything(), ~replace(., is.nan(.), NA))) |> 
    select(-cases:-pageviews_ceil, -n_articles, -n_studies),
  # calculate 7-day rolling averages for cases, news articles, & scientific studies
  mpox_test |> 
    distinct(date, cases, n_articles, n_studies) |> 
    mutate(
      roll_cases = slide_dbl(cases, .f = ~mean(.x, na.rm = TRUE), .before = 6),
      roll_n_articles = slide_dbl(n_articles, .f = ~mean(.x, na.rm = TRUE), .before = 6),
      roll_n_studies = slide_dbl(n_studies, .f = ~mean(.x, na.rm = TRUE), .before = 6)
    ) |> 
    select(date, starts_with("roll")),
  by = join_by(date)
) |> 
  select(-wikidata_id, -page_id) |> 
  pivot_wider(names_from = page_title, values_from = roll_pct_pageviews)

## Make Predictions ------------------------------------------------------------
# For all models
predictions <- map(models, ~predict(.x, newdata = mpox_test))

## Evaluate Predictions --------------------------------------------------------
# Calculate RMSE for the first model's predictions
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Calculate RMSE 
rmse_values <- map_dbl(predictions, ~rmse(mpox_test$roll_cases, .x))

# Visualize Predictions --------------------------------------------------------
plot_predictions <- function(predictions, actual_data, date_col, case_col, model_names = NULL) {
  # Check if model names are provided, if not generate default names
  if (is.null(model_names)) {
    model_names <- paste("Model", seq_along(predictions))
  }
  
  # Create a list of plots, one for each model
  plots <- map2(predictions, model_names, ~{
    ggplot(data = actual_data, aes(x = !!sym(date_col), y = !!sym(case_col))) +
      geom_line(color = "black") +
      geom_line(aes(y = .x), color = "red", linetype = "longdash") +
      labs(title = paste("Actual vs Predicted Cases -", .y),
           x = "Date", y = "Number of Cases") +
      scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
      theme_minimal()
  })
  
  return(plots)
}

# Assuming `mpox_test` contains 'date' as 'date' and 'roll_cases' as the actual outcomes
model_plots <- plot_predictions(
  predictions = predictions,
  actual_data = mpox_test,
  date_col = "date",
  case_col = "roll_cases"
)

# If you have specific names for models, pass them like this:
# model_names = c("Mpox Model", "Included Articles Model", ...)
walk(model_plots, print)

# Alternatively, save each plot to a file
#walk2(model_plots, seq_along(model_plots), ~ggsave(paste0("Model_", .y, ".png"), plot = .x, width = 10, height = 6))
