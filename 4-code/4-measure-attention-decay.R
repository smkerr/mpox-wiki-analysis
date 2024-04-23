# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================


# Setup ========================================================================
mpox_df <- read_csv(here("3-data/output/mpox-data.csv")) 

# TODO: Assume upper limit to attention decay? E.g., no more than 50 days or so?


# Prepare data =================================================================
attention_df <- mpox_df |>
  filter(page_title == "Mpox") |> 
  complete(
    date = seq.Date(from = min(mpox_df$date), to = max(mpox_df$date), by = 1),
    fill = list(country = "United States", iso2 = "US", iso3 = "USA")
    ) |> 
  mutate(
    time = row_number(),
    rolling_avg = slide_dbl(pct_pageviews, ~mean(.x, na.rm = TRUE), .before = 7, .complete = FALSE)
    )

# Viz check
ggplot(attention_df) + 
  #geom_line(aes(date, pct_pageviews), color = muted("red"), alpha = 0.9) + 
  geom_line(aes(time, rolling_avg), color = muted("blue"), alpha = 0.9) + 
  geom_vline(xintercept = 142, color = "red", linetype = "dashed") +
  geom_vline(xintercept = 215, color = "red", linetype = "dashed") +
  scale_y_continuous(labels = label_percent()) +
  theme_minimal()


# Peak pageviews ===============================================================
peak_dates <- attention_df |> 
  mutate(rolling_avg = ifelse(is.na(rolling_avg), 0, rolling_avg)) |> # can't handle NAs
  pull(rolling_avg) |> 
  findpeaks(nups = 2, ndowns = 2, npeaks = 2, minpeakdistance = 7) |> 
  as.data.frame() |> 
  select(value = V1, time = V2, peak_start = V3, peak_end = V4) |> 
  arrange(time)

# First peak ===================================================================
peak_df1 <- attention_df |> 
  filter(time >= peak_dates$time[1], time < peak_dates$peak_start[2]) |> 
  mutate(days_since_peak = row_number())

# Viz check
ggplot(peak_df1, aes(days_since_peak, rolling_avg)) +
  geom_point() +
  scale_y_continuous(labels = label_percent()) +
  theme_minimal()

# Fit a linear model
lm_model1 <- lm(rolling_avg ~ days_since_peak, data = peak_df1) # TODO: log?

# Apply segmented regression
seg_model1 <- segmented(lm_model1, seg.Z = ~days_since_peak, npsi = 2)
summary(seg_model1)
seg_model1$indexU$days_since_peak |> as.data.frame() 
peak_df1 |> 
  mutate(predicted = predict(seg_model1, newdata = data.frame(days_since_peak = days_since_peak))) |> 
  ggplot(aes(x = days_since_peak)) +
  geom_point(aes(y = rolling_avg), color = "blue", size = 2) +
  geom_line(aes(y = predicted), color = "red") +
  scale_y_continuous(labels = label_percent()) +
  labs(title = "Segmented Model Fit",
       x = "Days Since Peak",
       y = "Rolling Average/Predicted") +
  theme_minimal()
slope(seg_model1)


# Second peak ==================================================================
peak_df2 <- attention_df |> 
  filter(time >= peak_dates$time[2]) |> 
  mutate(days_since_peak = row_number())

# Viz check
ggplot(peak_df2, aes(days_since_peak, rolling_avg)) +
  geom_point() +
  scale_y_continuous(labels = label_percent()) +
  theme_minimal()

# Fit a linear model
lm_model2 <- lm(rolling_avg ~ days_since_peak, data = peak_df2) # TODO: log?

seg_model2 <- segmented(lm_model2, seg.Z = ~days_since_peak, npsi = 2)
summary(seg_model2)
seg_model2$indexU$days_since_peak |> as.data.frame() 
peak_df2 |> 
  mutate(predicted = predict(seg_model2, newdata = data.frame(days_since_peak = days_since_peak))) |> 
  ggplot(aes(x = days_since_peak)) +
  geom_point(aes(y = rolling_avg), color = "blue", size = 2) +
  geom_line(aes(y = predicted), color = "red") +
  scale_y_continuous(labels = label_percent()) +
  labs(title = "Segmented Model Fit",
       x = "Days Since Peak",
       y = "Rolling Average/Predicted") +
  theme_minimal()
slope(seg_model2)

# TODO: Save output 
# TODO: Add plots to figures script
