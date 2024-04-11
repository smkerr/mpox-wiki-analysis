# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================

# !!!At least in the US context, the correlation between cases and pageviews appears
# to be stronger at the weekly level as compared to the daily level. I'll go with 
# weekly for now...

# Setup ========================================================================
# load mpox cases & pageviews 
mpox_df <- read_csv(here("3-data/output/mpox-data.csv"))

# Calculate correlation between mpox cases and mpox-related pageviews by country
article_correlations <- mpox_df |>
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
                                                  use = "complete.obs"), 
                                .groups = 'drop') |> 
                              arrange(-correlation)
                            )) |> 
  select(-data) |> 
  unnest(correlations) |> 
  ungroup()
  
# Save results
write_csv(article_correlations, here("3-data/output/article-correlations.csv"))
