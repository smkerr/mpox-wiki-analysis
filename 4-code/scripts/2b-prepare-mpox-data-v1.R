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
  group_by(region, iso3, date) |> 
  summarize(cases = sum(cases, na.rm = TRUE)) |> 
  ungroup()

# order regions by total cases
cases_region_ordered <- cases_region_df |> 
  reframe(cases = sum(cases), .by = c(region)) |> 
  arrange(-cases)
cases_region_df <- cases_region_df |> 
  mutate(region = factor(region, levels = cases_region_ordered$region))

# daily country-level case data
cases_df <- cases_df |> 
  select(country = location, iso3 = iso_code, date, cases = new_cases, cases_moving_avg = new_cases_smoothed) |> 
  filter(!str_detect(iso3, "OWID")) # remove region-level counts

# weekly country-level case data 
cases_wk <- cases_df |> 
  mutate(date = ceiling_date(date, unit = "weeks", week_start = 3)) |> # calculate weekly cases
  group_by(country, iso3, date) |> 
  summarize(
    cases = sum(cases, na.rm = TRUE), 
    #cases_moving_avg = sum(cases_moving_avg, na.rm = TRUE) ### TODO: Valid method for aggregating moving average?
  ) |> 
  ungroup()


# Explore data =================================================================
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
library(ISOcodes)
library(sf)
library(spData)
library(tmap)

# get ISO2 <-> ISO3 conversions
iso_ref <- ISOcodes::ISO_3166_1 |> select(iso2 = Alpha_2, iso3 = Alpha_3) 

# calculate total cases by country 
cases_totals <- cases_df |> 
  reframe(cases = sum(cases), .by = c(country, iso3)) |> 
  left_join(iso_ref, by = join_by(iso3))

# set break values
break_values <- c(0, 10, 100, 1000, 10000, Inf)
left_join(world, cases_totals, by = join_by(iso_a2 == iso2)) |> # merge geometries with case data
  tm_shape() + # create map
  tm_polygons(fill = "cases", breaks = break_values, palette = "Blues", title = "Mpox cases")

cases_totals |> 
  arrange(-cases) |> 
  head(20)
