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
  ggplot2, 
  glue, 
  here, 
  lubridate, 
  purrr, 
  readr, 
  scales, 
  slider, 
  stringr,
  tidyr
  )

# Load data
mpox_df <- read_csv(here("3-data/output/mpox-data.csv")) |> 
  filter(date >= as_date("2022-05-10") & date <= as_date("2022-05-10") + days(180)) 
load(here("3-data/output/mpox-pages-included.RData"))


# Prepare data =================================================================
mpox_df <- left_join(
  # calculate 7-day rolling averages for pageviews 
  mpox_df |> 
    filter(page_title %in% included_articles) |> ###
    group_by(country, iso2, iso3, project, wikidata_id, page_id, page_title) |> 
    mutate(
      pageviews = ifelse(is.na(pageviews), 450, pageviews), ###
      pct_pageviews = pageviews / pageviews_ceil,
      roll_pct_pageviews = slide_dbl(pct_pageviews, ~mean(.x, na.rm = TRUE), .before = 6)
    ) |> 
    ungroup() |> 
    mutate(across(everything(), ~replace(., is.nan(.), NA))) |> 
    select(-cases:-n_studies),
  # calculate 7-day rolling averages for cases, news articles, & scientific studies
  mpox_df |> 
    distinct(date, cases, n_articles, n_studies) |> 
    mutate(
      roll_cases = slide_dbl(cases, .f = ~mean(.x, na.rm = TRUE), .before = 6),
      roll_n_articles = slide_dbl(n_articles, .f = ~mean(.x, na.rm = TRUE), .before = 6),
      roll_n_studies = slide_dbl(n_studies, .f = ~mean(.x, na.rm = TRUE), .before = 6)
    ) |> 
    select(date, starts_with("roll")),
  by = join_by(date)
  )


# Prepare data for lagged modeling =============================================
model_datasets <- list(
  # Model 1: "Mpox" pageviews
  "mpox" = mpox_df |> 
    select(-roll_n_articles, -roll_n_studies) |> 
    filter(page_title == "Mpox") |> 
    pivot_wider(names_from = page_title, values_from = roll_pct_pageviews),
  # Model 2: Included articles pageviews
  "included_articles" = mpox_df |> 
    select(-wikidata_id, -page_id, -roll_n_articles, -roll_n_studies) |> 
    pivot_wider(names_from = page_title, values_from = roll_pct_pageviews),
  # Model 3: "Mpox" pageviews + news + academia
  "mpox_covars" = mpox_df |> 
    filter(page_title == "Mpox") |> 
    pivot_wider(names_from = page_title, values_from = roll_pct_pageviews),
  # Model 4: Included articles pageviews + news + academia
  "included_articles_covars" = mpox_df |> 
    select(-wikidata_id, -page_id) |> 
    pivot_wider(names_from = page_title, values_from = roll_pct_pageviews) |> 
    arrange(date)
)


# Modeling Function ============================================================
lag_and_model <- function(data, lag) {
  # Order chronologically
  data <- data |> 
    arrange(date) 
  
  # Independent variables
  independent_vars <- setdiff(names(data), c("country", "iso2", "iso3", "project", "wikidata_id", "page_id", "date", "roll_cases"))
  independent_vars_quoted <- sprintf("`%s`", independent_vars) # add backticks to handle unconventional var names
  
  # Model formula
  model_formula <- as.formula(paste("roll_cases ~", paste(independent_vars_quoted, collapse = " + ")))
  
  # Shift the lagged variables
  if (lag > 0) {
    shifted_df <- data |> 
      mutate(across(all_of(independent_vars), ~lead(.x, abs(lag)))) |> 
      drop_na()
  } else if (lag < 0) {
    shifted_df <- data |> 
      mutate(across(all_of(independent_vars), ~lag(.x, abs(lag)))) |> 
      drop_na() 
  } else { # lag == 0
    shifted_df <- data
  }
  
  # Build the model
  model <- lm(model_formula, data = shifted_df)

  return(model)
}


# Apply Models Across Lags =====================================================
lags <- -37:37

# Model 1: "Mpox" pageviews
models_mpox <- map(lags, ~ lag_and_model(model_datasets$mpox, .x)) |> setNames(lags)

# Model 2: Included articles pageviews
models_included_articles <- map(lags, ~ lag_and_model(model_datasets$included_articles, .x)) |> setNames(lags)

# Model 3: "Mpox" pageviews + news + academia
models_mpox_covars <- map(lags, ~ lag_and_model(model_datasets$mpox_covars, .x)) |> setNames(lags)

# Model 4: Included articles pageviews + news + academia
models_included_articles_covars <- map(lags, ~ lag_and_model(model_datasets$included_articles_covars, .x)) |> setNames(lags)


# Evaluate Results =============================================================
## Visualize Adjusted R-squared and AIC ----------------------------------------
plot_model_results <- function(models, metric) {
  results <- map_df(models, glance) |> 
    mutate(lag = lags) |> 
    arrange(desc(!!sym(metric)))
  
  ggplot(results, aes(x = lag, y = !!sym(metric))) + 
    geom_line() + 
    geom_vline(xintercept = head(results$lag, 1), color = "red", linetype = "dashed") +
    scale_x_continuous(n.breaks = 15) +
    labs(title = paste("Model Metric:", metric)) +
    theme_minimal()
}

# Model 1: "Mpox" pageviews
plot_model_results(models_mpox, "adj.r.squared")
plot_model_results(models_mpox, "AIC")

# Model 2: Included articles pageviews
plot_model_results(models_included_articles, "adj.r.squared")
plot_model_results(models_included_articles, "AIC")

# Model 3: "Mpox" pageviews + news + academia
plot_model_results(models_mpox_covars, "adj.r.squared")
plot_model_results(models_mpox_covars, "AIC")

# Model 4: Included articles pageviews + news + academia
plot_model_results(models_included_articles_covars, "adj.r.squared")
plot_model_results(models_included_articles_covars, "AIC")


## Visualize Coefficients and p-values -----------------------------------------
plot_coef_results <- function(models, metric) {
  results <- map_df(models, tidy) |> 
    arrange(term) |> 
    mutate(
      lag = rep(lags, length(models$`0`$coefficients)),
      term = case_when(
        term == "roll_n_articles" ~ "Number of news articles",
        term == "roll_n_studies" ~ "Number of scientific studies",
        TRUE ~ term)
    ) |> 
    filter(term != "(Intercept)") |> 
    arrange(lag, term)
  
  ggplot(results, aes(x = lag, y = !!sym(metric), color = term)) + 
    geom_line() + 
    geom_hline(yintercept = ifelse(!!sym(metric) == "p.value", 0.05, 0), color = "red", linetype = "dashed") +
    facet_wrap(~term, scales = ifelse(!!sym(metric) == "p.value", "fixed", "free"), nrow = 3) + 
    scale_x_continuous(n.breaks = 15) +
    labs(title = metric, x = "Lag [days]", color = NULL) +
    theme_minimal() + 
    theme(legend.position = "none")
}

# Model 1: "Mpox" pageviews
plot_coef_results(models_mpox, "estimate")
plot_coef_results(models_mpox, "p.value")

# Model 2: Included articles pageviews
plot_coef_results(models_mpox_covars, "estimate")
plot_coef_results(models_mpox_covars, "p.value")

# Model 3: "Mpox" pageviews + news + academia
plot_coef_results(models_included_articles, "estimate")
plot_coef_results(models_included_articles, "p.value")

# Model 4: Included articles pageviews + news + academia
plot_coef_results(models_included_articles_covars, "estimate")
plot_coef_results(models_included_articles_covars, "p.value")

# TODO: Results may be better suited for a table

               
# Conclusion ===================================================================
#> Negative lags correspond with the time-lag effect of independent vars on cases
#> Positive lags correspond with the time-lag effect of cases on independent vars 


# Store results
#write_csv(model_results, here(glue("3-data/output/lag-analysis-results.csv")))
