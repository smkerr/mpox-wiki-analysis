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
  cowplot,
  dplyr, 
  ggplot2,
  glue, 
  gt,
  here, 
  lubridate, 
  purrr, 
  readr, 
  scales,
  slider,
  stringr,
  tidyr,
  install = FALSE
  )

# Load data 
mpox_train <- read_csv(here("3-data/output/mpox-data.csv")) |> 
  filter(date >= as_date("2022-05-10") & date <= as_date("2023-09-19")) 
mpox_test <- read_csv(here("3-data/output/mpox-data.csv")) |> 
  filter(date >= as_date("2023-09-20") & date <= as_date("2024-02-27")) 
load(here("3-data/output/article-selection/mpox-pages-included.RData"))


# Prepare data =================================================================
train_df <- left_join(
  # calculate 7-day rolling averages for pageviews 
  mpox_train |> 
    filter(page_title %in% c("Mpox", included_articles)) |> 
    # Expand to include missing dates
    complete(
      page_title = included_articles,
      date = seq.Date(from = min(mpox_train$date, na.rm = TRUE), to = max(mpox_train$date, na.rm = TRUE), by = 1),
    ) |> 
    # Fill in missing info
    fill(country, iso2, iso3, project, wikidata_id, page_id, page_title, pageviews_ceil, .direction = "downup") |> 
    mutate(
      pageviews = ifelse(is.na(pageviews) | pageviews == 0, 0, pageviews), ###
      pct_pageviews = pageviews / pageviews_ceil,
    ) |> 
    group_by(country, iso2, iso3, project, wikidata_id, page_id, page_title) |> 
    mutate(roll_pct_pageviews = slide_dbl(pct_pageviews, ~mean(.x, na.rm = TRUE), .before = 3, .after = 3)) |>
    ungroup() |> 
    mutate(across(everything(), ~replace(., is.nan(.), NA))) |>
    select(-cases:-pageviews_ceil, -n_articles, -n_studies),
  # Calculate 7-day rolling averages 
  mpox_train |> 
    distinct(date, cases, n_articles, n_studies) |> 
    mutate(
      roll_cases = slide_dbl(cases, .f = ~mean(.x, na.rm = TRUE), .before = 3, .after = 3),
      roll_n_articles = slide_dbl(n_articles, .f = ~mean(.x, na.rm = TRUE), .before = 3, .after = 3),
      roll_n_studies = slide_dbl(n_studies, .f = ~mean(.x, na.rm = TRUE), .before = 3, .after = 3)
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
  "mpox" = train_df |> 
    select(all_of(id_vars), date, roll_cases, Mpox),
  # Model 2: Included articles pageviews
  "included_articles" = train_df |> 
    select(all_of(id_vars), date, roll_cases, all_of(included_articles)),
  # Model 3: "Mpox" pageviews + news + academia
  "mpox_covars" = train_df |> 
    select(all_of(id_vars), date, starts_with("roll"), Mpox),
  # Model 4: Included articles pageviews + news + academia
  "included_articles_covars" = train_df |> 
    select(all_of(id_vars), date, starts_with("roll"), all_of(included_articles))
)


# Model Specification ==========================================================
included_articles_reformatted <- sprintf('`%s`', included_articles) # handle spaces in var names

formulas <- list(
  # Model 1: "Mpox" pageviews
  mpox = as.formula("roll_cases ~ Mpox"),
  # Model 2: Included articles pageviews
  included_articles = as.formula(glue("roll_cases ~ {paste(included_articles_reformatted, collapse = '+')}")),
  # Model 3: "Mpox" pageviews + news + academia
  mpox_covars = as.formula("roll_cases ~ Mpox + roll_n_articles + roll_n_studies"),
  # Model 4: Included articles pageviews + news + academia
  included_articles_covars = as.formula(glue("roll_cases ~ {paste(included_articles_reformatted, collapse = '+')} + roll_n_articles + roll_n_studies"))
)


# Fit Models ===================================================================
models <- map2(formulas, model_datasets, ~lm(.x, .y))


# Summarize Model Fit (Training Data) ==========================================
## Model summary ---------------------------------------------------------------
# Combine summaries into single dataframe
model_summary_table <- map(models, glance) |> 
  bind_rows(.id = "Model") |> 
  select(Model, r.squared, adj.r.squared, AIC, BIC) |> 
  pivot_longer(cols = -Model, names_to = "Fitting parameter", values_to = "Value") |> 
  pivot_wider(names_from = Model, values_from = Value) |> 
  mutate(
    `Fitting parameter` = case_when(
      `Fitting parameter`== "r.squared" ~ "R²",
      `Fitting parameter`== "adj.r.squared" ~ "Adjusted R²",
      TRUE ~ `Fitting parameter`
    )
  )

# Save summary data
write_csv(model_summary_table, here("3-data/output/regression-analysis/regression-models-summary.csv"))


## Coefficient summary ---------------------------------------------------------
# Combine coefficient estimates into single dataframe
coef_summary_table <- map(models, tidy) |> 
  bind_rows(.id = "Model")

# Save coefficient estimate summary data
write_csv(coef_summary_table, here("3-data/output/regression-analysis/regression-coef-summary.csv"))


# Model Diagnostics ============================================================
check_diagnostics <- function(model) {
  par(mfrow = c(2,2))
  plot(model)  # plots Residuals vs Fitted, Normal Q-Q, Scale-Location, Residuals vs Leverage
}

# Visualize diagnostic checks 
walk(models, check_diagnostics)


# Plot Model Fit ===============================================================
plot_model <- function(formula, data, description) {
  # Fit the model
  model <- lm(formula, data)
  
  # Augment the data with the model's fitted values and residuals
  augmented_data <- augment(model) |> 
    mutate(date = data$date)

  # Plotting
  p <- ggplot(augmented_data, aes(x = date)) +
    geom_line(aes(y = roll_cases), color = "black") +
    geom_line(aes(y = .fitted), color = "red", linetype = "longdash") +
    scale_x_date(
      limits = c(as_date("2022-05-01"), as_date("2023-09-19")), 
      date_breaks = "1 month",
      breaks = seq.Date(from = as_date("2022-05-01"), to = as_date("2023-09-19"), by = "3 months"), 
      date_labels = "%b\n%Y" ,
      expand = expansion()
    ) + 
    scale_y_continuous(breaks = pretty_breaks(n = 6)) +
    theme_minimal() +
    labs(
      title = description,
      x = NULL,
      y = "Cases"
      ) + 
    theme(
      plot.title = element_text(face = "bold"),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold")
    )
  
  return(p)
}

# Plot actual vs fitted cases
p1 <- plot_model(formulas[[1]], model_datasets[[1]], 'Mpox pageviews')
p2 <- plot_model(formulas[[2]], model_datasets[[2]], "Mpox-related pageviews")
p3 <- plot_model(formulas[[3]], model_datasets[[3]], "Mpox pageviews + media coverage + scientific interest")
p4 <- plot_model(formulas[[4]], model_datasets[[4]], "Mpox-related pageviews + media coverage + scientific interest")

# Combined plot
plot_grid(p1, p2, p3, p4, ncol = 1)

# Save plot
ggsave(here("6-figures/regression-analysis-model-fit-train.png"), height = 7.75, width = 10)

# Validate Forecast ============================================================
## Prepare data ----------------------------------------------------------------
test_df <- left_join(
  # calculate 7-day rolling averages for pageviews 
  mpox_test |> 
    filter(page_title %in% c("Mpox", included_articles)) |> 
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
    mutate(roll_pct_pageviews = slide_dbl(pct_pageviews, ~mean(.x, na.rm = TRUE), .before = 3, .after = 3)) |>
    ungroup() |> 
    mutate(across(everything(), ~replace(., is.nan(.), NA))) |> 
    select(-cases:-pageviews_ceil, -n_articles, -n_studies),
  # calculate 7-day rolling averages for cases, news articles, & scientific studies
  mpox_test |> 
    distinct(date, cases, n_articles, n_studies) |> 
    mutate(
      roll_cases = slide_dbl(cases, .f = ~mean(.x, na.rm = TRUE), .before = 3, .after = 3),
      roll_n_articles = slide_dbl(n_articles, .f = ~mean(.x, na.rm = TRUE), .before = 3, .after = 3),
      roll_n_studies = slide_dbl(n_studies, .f = ~mean(.x, na.rm = TRUE), .before = 3, .after = 3)
    ) |> 
    select(date, starts_with("roll")),
  by = join_by(date)
) |> 
  select(-wikidata_id, -page_id) |> 
  pivot_wider(names_from = page_title, values_from = roll_pct_pageviews)

## Make Predictions ------------------------------------------------------------
# For all models
predictions <- map(models, ~predict(.x, newdata = test_df))

## Evaluate Predictions --------------------------------------------------------
# Calculate RMSE for the first model's predictions
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Calculate RMSE 
rmse_values <- map_dbl(predictions, ~rmse(test_df$roll_cases, .x))

# Visualize Predictions --------------------------------------------------------
plot_predictions <- function(predicted_vals, data, description) {
  # Combine data with predictions
  augmented_data <- data |> 
    mutate(predicted = predicted_vals)
  
  # Plotting
  p <- ggplot(augmented_data, aes(x = date)) +
    geom_line(aes(y = roll_cases), color = "black") +
    geom_line(aes(y = predicted), color = "red", linetype = "longdash") +
    scale_x_date(
      limits = c(as_date("2023-09-20"), as_date("2024-02-27")), 
      date_breaks = "1 month",
      breaks = seq.Date(from = as_date("2023-09-20"), to = as_date("2024-02-27"), by = "1 month"), 
      date_labels = "%b\n%Y" ,
      expand = expansion()
    ) + 
    scale_y_continuous(breaks = pretty_breaks(n = 6)) +
    theme_minimal() +
    labs(
      title = description,
      x = NULL,
      y = "Cases"
    ) + 
    theme(
      plot.title = element_text(face = "bold"),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold")
    )
  
  return(p)
}

# Plot actual vs fitted cases
p1 <- plot_predictions(predictions[[1]], test_df, 'Mpox pageviews')
p2 <- plot_predictions(predictions[[2]], test_df, "Mpox-related pageviews")
p3 <- plot_predictions(predictions[[3]], test_df, "Mpox pageviews + media coverage + scientific interest")
p4 <- plot_predictions(predictions[[4]], test_df, "Mpox-related pageviews + media coverage + scientific interest")

# Combined plot
plot_grid(p1, p2, p3, p4, ncol = 1)

# Save plot
ggsave(here("6-figures/regression-analysis-model-fit-test.png"), height = 7.75, width = 10)
