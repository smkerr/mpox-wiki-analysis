# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================

library(segmented)

# Find date of peak pageviews 
date_max_pageviews <- mpox_df |> 
  filter(pageviews_est == max(pageviews_est)) |> 
  pull(date)
date_max_pageviews <- as_date("2022-07-23")

# Prepare data 
mpox_attention <- mpox_df |> 
  group_by(country) |> 
  mutate(days_since_event = as.numeric(date - date_max_pageviews))

# Fit a linear model
lm_model <- lm(pageviews_est ~ days_since_event, data = mpox_attention)

# Apply segmented regression
seg_model <- segmented(lm_model, seg.Z = ~days_since_event, psi = c(1, 14))

# Analyze results
# View breakpoints and their confidence intervals
summary(seg_model)
slope(seg_model)

# Plot the model
plot(seg_model)

