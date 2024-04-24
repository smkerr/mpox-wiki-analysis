# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================


# Setup ========================================================================
# Load packages 
pacman::p_load(dplyr, here, readr, purrr)
# load mpox cases & pageviews 
mpox_df <- read_csv(here("3-data/output/mpox-data.csv"))

# The purpose of this section is to determine which articles that we've flagged 
# as potentially relevant are in fact correlated with cases 
# Of these, some sort of selection criteria will be implemented to decide 
# which articles' pageviews will be included in the lag analysis and time-series 
# analysis sections


# Calculate Spearman correlation between cases and pageviews ===================
article_correlations <- mpox_df |>
  filter(date >= as_date("2022-05-10") & date <= as_date("2022-05-10") + days(180)) |> 
  group_by(country, iso2, iso3, wikidata_id, page_id, page_title) |>
  filter(sum(pct_pageviews > 0, na.rm = TRUE) > 1) |> # remove articles estimated to have zero pageviews
  ungroup() |>
  group_by(country, iso2, iso3) |>
  nest() |>
  mutate(correlations = map(data, ~ .x |>
                              group_by(page_title) |>
                              summarize(
                                n = n(),
                                correlation = cor(pct_pageviews,
                                                  cases, # TODO: logged cases instead???
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
write_csv(article_correlations, here("3-data/output/article-correlations.csv"))


# Implement inclusion criteria =================================================
#> In order to mininze the amount of spurious correlation, we'll identify results 
#> that are shown to have some level of positive correlation using either method 
#> (Pearson or Spearman) and at least 30 observations

# Filter for positive correlation
included_articles <- article_correlations |> 
  filter(
    n >= 30,
    correlation > 0
    ) |> 
  pull(page_title)


# Save results =================================================================
save(included_articles, file = here("3-data/output/mpox-pages-included.RData"))
# TODO: Save as excel or .txt file to make it easier for reviewer?
