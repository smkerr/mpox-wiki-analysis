# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================


# Load data ====================================================================
# load mpox case data
cases_df <- read_csv("https://raw.githubusercontent.com/owid/monkeypox/main/owid-monkeypox-data.csv")
write_csv(cases_df, here(glue("3-data/mpox-cases/owid-monkeypox-data ({format(today(), '%Y-%m-%d')}).csv")))

# Prepare data =================================================================
# daily region-level case data
cases_region_df <- cases_df |> 
  select(region = location, iso3 = iso_code, date, cases = new_cases, cases_moving_avg = new_cases_smoothed) |> 
  filter(str_detect(iso3, "OWID"), region != "World") |> # only keep region-level counts
  mutate(date = ceiling_date(date, unit = "weeks", week_start = 3)) |> # calculate weekly cases
  reframe(cases = sum(cases, na.rm = TRUE), .by = c(region, iso3, date)) |> 
  ungroup()

# order regions by total cases
cases_region_df <- cases_region_df |> 
  mutate(region = factor(region, 
                         levels = cases_region_df |> 
                           reframe(cases = sum(cases), .by = c(region)) |> 
                           arrange(-cases) |> 
                           pull(region)
                         )
         )

# daily country-level case data
cases_df <- cases_df |> 
  select(country = location, iso3 = iso_code, date, cases = new_cases, cases_moving_avg = new_cases_smoothed) |> 
  filter(!str_detect(iso3, "OWID")) # remove region-level counts

# weekly country-level case data 
cases_wk <- cases_df |> 
  mutate(date = ceiling_date(date, unit = "weeks", week_start = 3)) |> # calculate weekly cases
  group_by(country, iso3, date) |> 
  reframe(cases = sum(cases, na.rm = TRUE)) |>
  ungroup()

# Impute missing data ==========================================================
### test 
cases_wk <- cases_wk |> filter(iso3 == "USA")
date_sequence <- seq.Date(from = min(cases_wk$date), to = max(cases_wk$date), by = "day")
cases_wk <- cases_wk |> 
  complete(date = date_sequence, fill = list(value = NA)) |> 
  fill(c(country, iso3), .direction = "down")



# # Fit a spline
# fitted_spline <- spline(x = cases_wk$date, y = cases_wk$cases, xout = date_range)
# 
# # Create a new data frame with the imputed data
# imputed_data <- data.frame(date = as_date(fitted_spline$x), cases = fitted_spline$y)


library(imputeTS)

# Assuming weekly_data is your dataset and it has NA values for missing days
cases_wk$imputed_series <- na_interpolation(cases_wk$cases, option = "spline")

# ggplot() +
#   geom_line(data = cases_wk, aes(x = date, y = imputed_series), color = "blue", linetype = "dashed") +
#   geom_point(data = cases_wk, aes(x = date, y = cases), color = "red") +
#   labs(title = "Weekly Disease Cases and Imputed Daily Data",
#        x = "Date", y = "Cases") +
#   theme_minimal()

library(zoo)

# Assuming weekly_data is your dataset
zoo_object <- zoo(cases_wk$cases, order.by = cases_wk$date)
cases_wk$imputed_data <- na.spline(zoo_object, xout = date_sequence)

ggplot() +
  geom_line(data = cases_wk, aes(x = date, y = imputed_data), color = "blue", linetype = "dashed") +
  geom_point(data = cases_wk, aes(x = date, y = cases), color = "red") +
  labs(title = "Weekly Disease Cases and Imputed Daily Data",
       x = "Date", y = "Cases") +
  theme_minimal()

library(tidyverse)

# Assuming weekly_totals from the previous example
# We'll create a simple model based on the assumption that the number of cases grows linearly

# Create a simple linear model of total_cases as a function of week number
model <- lm(cases ~ date, data = cases_wk)

# Predict the total cases for each week
predictions <- predict(model, cases_wk)

# Visualize the fit
ggplot(cases_wk) +
  geom_col(aes(x = date, y = cases / 200000), alpha = 0.5) +
  geom_point(aes(x = date, y = cases / 200000), alpha = 0.5) +
  geom_density(aes(x = date, y = after_stat(density), weight = cases), adjust = 0.15, alpha = 0.5) +
  scale_x_date() +
  labs(y = "Density", title = "Density of mpox cases over time") +
  theme_minimal()




# Visualize data ===============================================================
# plot daily cases
cases_df |> 
  reframe(cases = sum(cases), .by = date) |> 
  ggplot(aes(x = date, y = cases)) + 
  geom_col(col = "black", width = 0.9) +
  expand_limits(y = 5) +
  scale_x_date(
    limits = c(as_date("2022-01-01"), max(cases_df$date)), 
    expand = expansion(mult = 0.05),
    date_breaks = "3 months",
    date_labels = "%b\n%Y" 
  ) + 
  scale_y_continuous(
    limits = c(0, NA), 
    expand = expansion(mult = c(0.02, 0.02)), 
    breaks = pretty_breaks(),
    labels = comma_format()
  ) +
  labs(
    title = "Mpox epidemic curve",
    #subtitle = "data as of 31 Jan 2024 17:00 CET",
    x = "Date reported",
    y = "Cases",
    caption = "Source: World Health Organization via Our World in Data",
    fill = NULL
  ) +
  theme_minimal() + 
  theme(
    legend.position = "none",
    plot.margin = unit(c(0.2, 1, 0.2, 0.2), units = "cm")
  )

# save plot
ggsave(here("5-visualization/mpox-cases-daily.png"), height = 7.75, width = 10)


# plot 7-day average cases
cases_df |> 
  reframe(cases_moving_avg = sum(cases_moving_avg), .by = date) |> 
  ggplot(aes(x = date, y = cases_moving_avg)) + 
  geom_col(col = "black", width = 0.5) +
  expand_limits(y = 5) +
  scale_x_date(
    limits = c(as_date("2022-01-01"), max(cases_df$date)), 
    expand = expansion(mult = 0.05),
    date_breaks = "3 months",
    date_labels = "%b\n%Y" 
  ) + 
  scale_y_continuous(
    limits = c(0, NA), 
    expand = expansion(mult = c(0.02, 0.02)), 
    breaks = pretty_breaks(),
    labels = comma_format()
  ) +
  labs(
    title = "Mpox epidemic curve, 7-day average cases",
    x = "Date reported",
    y = "Cases",
    caption = "Source: World Health Organization via Our World in Data",
    fill = NULL
  ) +
  theme_minimal() + 
  theme(
    legend.position = "none",
    plot.margin = unit(c(0.2, 1, 0.2, 0.2), units = "cm")
  )

# save plot
ggsave(here("5-visualization/mpox-cases-7day-avg.png"), height = 7.75, width = 10)


# plot weekly cases
cases_wk |> 
  reframe(cases = sum(cases), .by = date) |> 
  ggplot(aes(x = date, y = cases)) + 
  geom_col(col = "black", width = 7) +
  expand_limits(y = 5) +
  scale_x_date(
    limits = c(as_date("2022-01-01"), max(cases_df$date)), 
    expand = expansion(mult = 0.05),
    date_breaks = "3 months",
    date_labels = "%d %b\n%Y" 
  ) + 
  scale_y_continuous(
    limits = c(0, NA), 
    expand = expansion(mult = c(0.02, 0.02)), 
    breaks = pretty_breaks(),
    labels = comma_format()
  ) +
  labs(
    title = "Mpox epidemic curve",
    #subtitle = "data as of 31 Jan 2024 17:00 CET",
    x = "Week reported",
    y = "Cases",
    caption = "Source: World Health Organization via Our World in Data",
    fill = NULL
  ) +
  theme_minimal() + 
  theme(
    legend.position = "none",
    plot.margin = unit(c(0.2, 1, 0.2, 0.2), units = "cm")
  )

# save results
ggsave(here("5-visualization/mpox-cases-weekly.png"), height = 7.75, width = 10)


# plot weekly cases by region 
cases_region_df |> 
  reframe(cases = sum(cases), .by = c(region, date)) |> 
  ggplot(aes(x = date, y = cases, fill = fct_rev(region))) + 
  geom_col(col = "black", width = 7) +
  expand_limits(y = 5) +
  scale_x_date(
    limits = c(as_date("2022-01-01"), max(cases_region_df$date)), 
    expand = expansion(mult = 0.05),
    date_breaks = "3 months",
    date_labels = "%d %b\n%Y" 
  ) + 
  scale_y_continuous(
    limits = c(0, NA), 
    expand = expansion(mult = c(0.02, 0.02)), 
    breaks = pretty_breaks(),
    labels = comma_format()
  ) +
  scale_fill_brewer(type = "qual", palette = 3, direction = -1) +
  labs(
    title = "Mpox epidemic curve, by region",
    #subtitle = "data as of 31 Jan 2024 17:00 CET",
    x = "Week reported",
    y = "Cases",
    caption = "Source: World Health Organization via Our World in Data",
    fill = NULL
  ) +
  theme_minimal() + 
  theme(
    legend.position = "right",
    plot.margin = unit(c(0.2, 1, 0.2, 0.2), units = "cm")
  )

# save results
ggsave(here("5-visualization/mpox-cases-weekly-region.png"), height = 7.75, width = 14)


# plot weekly cases by region 
cases_region_df |> 
  reframe(cases = sum(cases), .by = c(region, date)) |> 
  ggplot(aes(x = date, y = cases, fill = fct_rev(region))) + 
  geom_col(col = "black", width = 7) +
  expand_limits(y = 5) +
  scale_x_date(
    limits = c(as_date("2022-01-01"), max(cases_region_df$date)), 
    expand = expansion(mult = 0.05),
    date_breaks = "3 months",
    date_labels = "%b\n%Y" 
  ) + 
  scale_y_continuous(
    limits = c(0, NA), 
    expand = expansion(mult = c(0.02, 0.02)), 
    breaks = pretty_breaks(),
    labels = comma_format()
  ) +
  scale_fill_brewer(type = "qual", palette = 3, direction = -1) +
  facet_wrap(~region, ncol = 1, scales = "free_y") +
  labs(
    title = "Mpox epidemic curve, by region",
    #subtitle = "data as of 31 Jan 2024 17:00 CET",
    x = "Week reported",
    y = "Cases",
    caption = "Source: World Health Organization via Our World in Data",
    fill = NULL
  ) +
  theme_minimal() + 
  theme(
    legend.position = "none",
    plot.margin = unit(c(0.2, 1, 0.2, 0.2), units = "cm")
  )

# save results
ggsave(here("5-visualization/mpox-cases-weekly-region-facetted.png"), height = 7.75, width = 10)


# Map of cumulative cases
# get ISO2 <-> ISO3 conversions
iso_ref <- ISOcodes::ISO_3166_1 |> select(iso2 = Alpha_2, iso3 = Alpha_3) 

# calculate total cases by country 
cases_totals <- cases_df |> 
  reframe(cases = sum(cases), .by = c(country, iso3)) |> 
  left_join(iso_ref, by = join_by(iso3))

# set break values
data(World)
break_values <- c(0, 10, 100, 1000, 10000, Inf)
left_join(World, cases_totals, by = join_by(iso_a3 == iso3)) |> # merge geometries with case data
  tm_shape() + # create map
  tm_polygons(fill = "cases", breaks = break_values, palette = "Blues", title = "Mpox cases")

cases_totals |> 
  arrange(-cases) |> 
  head(20)
