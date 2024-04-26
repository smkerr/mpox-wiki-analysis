# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Author: Steve Kerr
# Date: April 2024
# ==============================================================================


# Setup ========================================================================
# Load packages 
pacman::p_load(dplyr, here, readr, purrr, tibble)

# Load data
mpox_df <- read_csv(here("3-data/output/mpox-data.csv")) |> 
  filter(date >= as_date("2022-05-10") & date <= as_date("2023-02-05"))

# The purpose of this section is to determine which articles that we've flagged 
# as potentially relevant are in fact correlated with cases 
# Of these, some sort of selection criteria will be implemented to decide 
# which articles' pageviews will be included in the lag analysis and time-series 
# analysis sections


# Calculate Spearman correlation between cases and pageviews ===================
article_correlations <- mpox_df |>
  group_by(country, iso2, iso3, wikidata_id, page_id, page_title) |>
  filter(sum(pct_pageviews > 0, na.rm = TRUE) > 1) |> # remove articles with zero pageviews
  ungroup() |>
  group_by(country, iso2, iso3) |>
  nest() |>
  mutate(correlations = map(data, ~ .x |>
                              group_by(page_title) |>
                              summarize(
                                n = n(),
                                correlation = cor(pct_pageviews,
                                                  cases, 
                                                  use = "complete.obs",
                                                  method = "spearman"),
                                method = "spearman",
                                .groups = 'drop') |>
                              arrange(-correlation)
  )) |>
  select(-data) |>
  unnest(correlations) |>
  ungroup()

# Save results
write_csv(article_correlations, here("3-data/output/article-selection/article-correlations.csv"))


# Implement inclusion criteria =================================================
#> In order to minimize the amount of spurious correlation, we take articles 
#> where the absolute value of correlation is > 0.5 with at least 100 observations

# Filter for positive correlation
included_articles <- article_correlations |> 
  mutate(correlation = abs(correlation)) |> 
  filter(n >= 100, correlation > 0.5) |> 
  pull(page_title)


# Save results =================================================================
save(included_articles, file = here("3-data/output/article-selection/mpox-pages-included.RData"))
