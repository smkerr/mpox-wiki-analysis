# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================


# Setup ========================================================================
# load mpox cases & pageviews 
mpox_df <- read_csv(here("3-data/output/mpox-data.csv"))

# The purpose of this section is to determine which articles that we've flagged 
# as potentially relevant are in fact correlated with cases 
# Of these, some sort of selection criteria will be implemented to decide 
# which articles' pageviews will be included in the lag analysis and time-series 
# analysis sections


# Calculate Pearson correlation between cases and pageviews ====================
article_correlations_pearson <- mpox_df |>
  group_by(country, iso2, iso3, wikidata_id, page_id, page_title) |>
  filter(sum(pct_pageviews > 0) > 1) |> # remove articles estimated to have zero pageviews
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
                                                  method = "pearson"),
                                method = "pearson",
                                .groups = 'drop') |>
                              arrange(-correlation)
                            )) |>
  select(-data) |>
  unnest(correlations) |>
  ungroup()

# Save results
write_csv(article_correlations_pearson, here("3-data/output/article-correlations-pearson.csv"))


# Calculate Spearman correlation between cases and pageviews ===================
article_correlations_spearman <- mpox_df |>
  group_by(country, iso2, iso3, wikidata_id, page_id, page_title) |>
  filter(sum(pct_pageviews > 0) > 1) |> # remove articles estimated to have zero pageviews
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
write_csv(article_correlations_spearman, here("3-data/output/article-correlations-spearman.csv"))


# Implement inclusion criteria =================================================
#> In order to mininze the amount of spurious correlation, we'll identify results 
#> that are shown to have some level of positive correlation using either method 
#> (Pearson or Spearman) and at least 30 observations

# Filter for positive correlation
included_articles <- bind_rows(article_correlations_pearson, article_correlations_spearman) |> 
  filter(n >= 30) |> 
  group_by(method) |> 
  filter(correlation > 0) |> 
  ungroup() |> 
  count(page_title) |> 
  filter(n == 2) |> 
  pull(page_title)


# Save results =================================================================
save(included_articles, file = here("3-data/output/mpox-pages-included.RData"))
# TODO: Save as excel or .txt file to make it easier for reviewer?
