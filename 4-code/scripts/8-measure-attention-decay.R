# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================

### model attention decay at the global level
test_df <- pageviews_df |> 
  group_by(country, iso2, iso3, date) |> 
  summarize(
    est_pageviews = sum(est_pageviews),
    pct_est_pageviews = est_pageviews / pageviews_ceil
  ) |> 
  ungroup() |> 
  distinct()

# Max views --------------------------------------------------------------------
# Find date of peak pageviews 
date_max_pageviews <- test_df |> 
  filter(est_pageviews == max(est_pageviews)) |> 
  pull(date)

# PHEIC declaration ------------------------------------------------------------
date_max_pageviews <- as_date("2022-07-23")

# Prepare data 
test_df <- test_df |> 
  group_by(country) |> 
  filter(date >= date_max_pageviews) |>
  mutate(days_since_event = as.numeric(date - date_max_pageviews)) |> 
  filter(days_since_event <= 50) |> 
  ungroup()

# Fit a linear model
lm_model <- lm(pct_est_pageviews ~ days_since_event, data = test_df)

# Apply segmented regression
seg_model <- segmented(lm_model, seg.Z = ~days_since_event, psi = c(1, 7))

# Analyze results
# View breakpoints and their confidence intervals
summary(seg_model)
slope(seg_model)

# Plot the model
plot(seg_model)

# Generalize to multiple countries ---------------------------------------------
results <- test_df |>
  group_by(iso3) |>
  nest() |>
  mutate(
    Model = map(data, ~{
      if (nrow(.x) > 14) { # Only proceed if the group has more than min_obs observations
        lm_model <- lm(pct_est_pageviews ~ days_since_event, data = .x)
        tryCatch({
          seg_model <- segmented(lm_model, seg.Z = ~ days_since_event, psi = list(days_since_event = c(7, 14)))
          return(seg_model)
        }, error = function(e) {
          return(NULL) # Handle error, potentially return NULL or a different placeholder
        })
      } else {
        return(NULL) # Return NULL or another placeholder for groups with insufficient data
      }
    })
  )


# To get summaries for each country
results$Summary <- map(results$Model, summary)

# To plot the segmented regression for each country
# walk(results$Model, ~{
#   plot(.x)
#   title(paste("Segmented Regression for", unique(.x$data$Country)))
# })

plot(results$Model[[1]])

# Combine results for all countries --------------------------------------------
breakpoint_df <- data.frame()
for (iso3_code in results$iso3) {
  # Extract the summary object
  summary_obj <- results %>%
    filter(iso3 == iso3_code) |> 
    pull(Summary) |> 
    pluck(1)
  
  if ("psi" %in% names(summary_obj)) {
    psi_info <- summary_obj$psi |> 
      as.data.frame(optional = TRUE) |> 
      rownames_to_column("breakpoints") |> 
      select(breakpoints, `Est.`) |> 
      pivot_wider(names_from = "breakpoints", values_from = "Est.") |> 
      mutate(iso3 = iso3_code) |> 
      relocate(iso3, .before = everything())
    breakpoint_df <- bind_rows(breakpoint_df, psi_info)
  }
}
breakpoint_df

# Map breakpoints
attention_decay_df <- left_join(World, breakpoint_df, by = join_by(iso_a3 == iso3)) 
## first breakpoint
qtm(attention_decay_df, fill = "psi1.days_since_event")
hist(attention_decay_df$psi1.days_since_event, breaks = 25)

## second breakpoint
qtm(attention_decay_df, fill = "psi2.days_since_event")
hist(attention_decay_df$psi2.days_since_event, breaks = 20)
