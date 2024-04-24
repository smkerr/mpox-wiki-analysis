# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================


# Setup ========================================================================
# Load packages
pacman::p_load(
  MASS,
  dplyr,
  ggplot2,
  here,
  pracma,
  readr,
  scales,
  segmented,
  slider,
  tidyr
)

# Load data
mpox_df <- read_csv(here("3-data/output/mpox-data.csv")) 


# Prepare data =================================================================
# TODO: Consider shortening study period to May 2022 - ?
# TODO: Consider sharing this data preparation step across analysis scripts
attention_df <- mpox_df |>
  filter(page_title == "Mpox") |> 
  complete(
    date = seq.Date(from = min(mpox_df$date), to = max(mpox_df$date), by = 1), # complete missing dates
    fill = list(cases = 0) # missing case values are treated as zeros
    ) |> 
  # complete missing values for other variables
  fill(country, iso2, iso3, project, wikidata_id, page_title, page_id, .direction = "updown") |> 
  mutate(
    time = row_number(), # segmented wants date formatted as numeric value
    roll_pct_pageviews = slide_dbl(pct_pageviews, ~mean(.x, na.rm = TRUE), .before = 6, .complete = FALSE) # calculate 7-day rolling avg
    ) |> 
  select(country, iso2, iso3, project, wikidata_id, page_id, page_title, date, time, roll_pct_pageviews)


# Peak pageviews ===============================================================
peak_dates <- attention_df |> 
  mutate(roll_pct_pageviews = ifelse(is.na(roll_pct_pageviews), 0, roll_pct_pageviews)) |> # NAs to zeros
  pull(roll_pct_pageviews) |> 
  findpeaks(nups = 2, ndowns = 2, npeaks = 2, minpeakdistance = 7) |> 
  as.data.frame() |> 
  select(value = V1, time = V2, peak_start = V3, peak_end = V4) |> 
  arrange(time)

# Viz check
ggplot(attention_df) + 
  geom_line(aes(time, roll_pct_pageviews), alpha = 0.9) + 
  geom_vline(xintercept = peak_dates$time[1], color = "red", linetype = "dashed") + # first peak
  geom_vline(xintercept = peak_dates$time[2], color = "red", linetype = "dashed") + # second peak
  scale_y_continuous(labels = label_percent()) +
  theme_minimal()


# First peak ===================================================================
peak_df1 <- attention_df |> 
  filter(time >= peak_dates$time[1], time < peak_dates$peak_start[2]) |> 
  mutate(days_since_peak = row_number())

# Viz check
ggplot(peak_df1, aes(x = days_since_peak, y = roll_pct_pageviews)) +
  geom_point() +
  scale_y_continuous(labels = label_percent()) +
  theme_minimal()

# Viz check 
ggplot(peak_df1, aes(x = roll_pct_pageviews)) +
  geom_histogram(bins = 10) +
  theme_minimal()

# Viz check
ggplot(peak_df1, aes(log(roll_pct_pageviews))) +
  geom_histogram(bins = 10) +
  theme_minimal()

# Fit a linear model
lm_model1 <- lm(log(roll_pct_pageviews) ~ days_since_peak, data = peak_df1) 

# Apply segmented regression
seg_model1 <- segmented(
  lm_model1, 
  seg.Z = ~days_since_peak, 
  control = seg.control(n.boot = 500)
  )

# Model summary
summary(seg_model1)

# Visualize model fit
peak_df1 |> 
  mutate(predicted = predict(seg_model1, newdata = data.frame(days_since_peak = days_since_peak))) |> 
  ggplot(aes(x = days_since_peak)) +
  geom_point(aes(y = log(roll_pct_pageviews)), color = "blue", size = 2) +
  geom_line(aes(y = predicted), color = "red") +
  labs(title = "Segmented Model Fit",
       x = "Days Since Peak",
       y = "Rolling Average/Predicted") +
  theme_minimal()

# Check model diagnostics 
# Residuals plot
p1_res <- ggplot(data.frame(residuals = residuals(seg_model1)), aes(x = residuals)) +
  geom_histogram(bins = 30, fill = "grey", color = "black") +
  geom_vline(xintercept = 0, color = "red") +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency") +
  theme_minimal()
p1_res

# QQ plot
p1_qq <- ggplot(data.frame(residuals = residuals(seg_model1)), aes(sample = residuals)) +
  geom_qq() +
  geom_qq_line(color = "red") +
  labs(title = "QQ Plot of Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()
p1_qq


# Second peak ==================================================================
peak_df2 <- attention_df |> 
  filter(time >= peak_dates$time[2]) |> 
  mutate(days_since_peak = row_number()) |> 
  filter(days_since_peak <= 50) # only include first 50 days

# Viz check
ggplot(peak_df2, aes(days_since_peak, roll_pct_pageviews)) +
  geom_point() +
  scale_y_continuous(labels = label_percent()) +
  theme_minimal()

# Viz check 
# Skewed distribution
ggplot(peak_df2, aes(roll_pct_pageviews)) +
  geom_histogram(bins = 10) +
  theme_minimal()

# Viz check
# Logging results in more normal distribution
ggplot(peak_df2, aes(log(roll_pct_pageviews))) +
  geom_histogram(bins = 10) +
  theme_minimal()

# Fit a linear model
lm_model2 <- lm(log(roll_pct_pageviews) ~ days_since_peak, data = peak_df2) 

# Apply segmented regression
seg_model2 <- segmented(
  lm_model2, 
  seg.Z = ~days_since_peak, 
  control = seg.control(n.boot = 500)
  )

# Model summary
summary(seg_model2)

# Visualize model fit
peak_df2 |> 
  mutate(predicted = predict(seg_model2, newdata = data.frame(days_since_peak = days_since_peak))) |> 
  ggplot(aes(x = days_since_peak)) +
  geom_point(aes(y = log(roll_pct_pageviews)), color = "blue", size = 2) + 
  geom_line(aes(y = predicted), color = "red") +
  labs(title = "Segmented Model Fit",
       x = "Days Since Peak",
       y = "Rolling Average/Predicted") +
  theme_minimal()

# Check model diagnostics 
# Residuals plot
p2_res <- ggplot(data.frame(residuals = residuals(seg_model2)), aes(x = residuals)) +
  geom_histogram(bins = 30, fill = "grey", color = "black") +
  geom_vline(xintercept = 0, color = "red") +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency") +
  theme_minimal()
p2_res

# QQ plot
p2_qq <- ggplot(data.frame(residuals = residuals(seg_model2)), aes(sample = residuals)) +
  geom_qq() +
  geom_qq_line(color = "red") +
  labs(title = "QQ Plot of Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()
p2_qq

# Save models and data frames
save(seg_model1, seg_model2, file = here("3-data/output/segmented-analysis/segmented_models.RData"))
write.csv(attention_df, here("3-data/output/segmented-analysis/segmented-analysis-data.csv"))
