# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Author: Steve Kerr
# Date: April 2024
# ==============================================================================


# Setup ========================================================================
# Load packages 
pacman::p_load(MASS, dplyr, ggplot2, here, nortest, readr, scales)

# Load data 
load(here("3-data/ref/iso_codes.RData"))
mpox_df <- read_csv(here("3-data/output/mpox-data.csv"))

# Analysis =====================================================================
## Functions for Normality Testing ---------------------------------------------
hist_pageviews <- function(data) {
  data |> 
    filter(pct_pageviews > 0) |> 
    ggplot(aes(x = pct_pageviews)) + 
    geom_histogram(bins = 30, color = "black") +
    facet_wrap(~page_title, scale = "free") +
    scale_x_continuous(labels = label_percent()) +
    theme_minimal()
}

qq_pageviews <- function(data) {
  data |> 
    filter(pct_pageviews > 0) |> 
    ggplot(aes(sample = pct_pageviews)) + 
    geom_qq() + 
    geom_qq_line() +
    facet_wrap(~page_title, scale = "free") +
    theme_minimal()
}

normality_tests <- function(data, var) {
  ad_result <- ad.test(data[[var]])
  lilliefors_result <- lillie.test(data[[var]])
  shapiro_result <- shapiro.test(data[[var]])
  tibble(
    ad_p_value = ad_result$p.value,
    lilliefors_p_value = lilliefors_result$p.value,
    shapiro_p_value = shapiro_result$p.value
  )
}

## Visualize Distributions -----------------------------------------------------
hist_pageviews(mpox_df)
qq_pageviews(mpox_df)


## Normality Testing for Pageviews ---------------------------------------------
pageviews_results <- mpox_df |> 
  filter(pct_pageviews > 0) |> 
  group_by(page_title) |> 
  filter(n() > 7) |> # min number of observations required
  group_modify(~normality_tests(data = .x, var = "pct_pageviews")) |> 
  ungroup()
pageviews_results |> 
  filter(if_any(where(is.numeric), ~ . > 0.01))

## Normality Testing for Mpox Cases --------------------------------------------
cases_results <- mpox_df |> 
  distinct(country, date, cases) |> 
  group_by(country) |> 
  filter(sum(cases) > 30, cases > 0) |> 
  ungroup() |> 
  normality_tests(var = "cases")
cases_results |> 
  filter(if_any(where(is.numeric), ~ . > 0.01))

# Conclusion ===================================================================
#> According to all normality tests performed, while some mpox-related pageviews 
#> do meet the normality assumption, most pageviews variables as well as mpox 
#> cases fail to meet the assumption that they are normally distributed. 
#> Therefore the Pearson correlation coefficient is not appropriate for this 
#> context. Instead, the Spearman correlation can be used.
