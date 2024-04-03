# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================

# At least in the US context, the correlation between cases and pageviews appears
# to be stronger at the weekly level as compared to the daily level. I'll go with 
# weekly for now...

# Calculate correlation between mpox cases and mpox-related pageviews by country
article_correlations <- mpox_wk |>
  group_by(country, iso2, iso3, page_title) |> 
  filter(sum(pct_est_pageviews > 0) > 0) |> # remove articles estimated to have zero pageviews
  ungroup() |> 
  group_by(country, iso2, iso3) |> 
  nest() |> 
  mutate(correlations = map(data, ~ .x |> group_by(page_title) |> 
                              summarize(
                                correlation = cor(pct_est_pageviews, 
                                                  cases_moving_avg, 
                                                  use = "complete.obs"), 
                                .groups = 'drop') |> 
                              arrange(-correlation)
                            )) |> 
  select(-data) |> 
  unnest(correlations) |> 
  ungroup()

# Save results
write_csv(article_correlations, here("3-data/output/article-correlations.csv"))

# Select top 10 articles by greatest absolute Pearson correlation coefficient for each country
top_mpox_pages <- article_correlations |> 
  mutate(abs_correlation = abs(correlation)) |> 
  arrange(country, -abs_correlation) |> 
  group_by(country, iso2, iso3) |> 
  slice_head(n = 10) |> 
  ungroup() |> 
  select(-abs_correlation)

# Save results
save(top_mpox_pages, file = here("3-data/output/top-relevant-articles.RData"))

# Which of the most significant articles are most common?
top_mpox_pages |> 
  count(page_title, sort = TRUE) 
