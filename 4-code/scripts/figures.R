# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================


# Load data ====================================================================
# load pageviews data
pageviews <- read_csv(here("3-data/wikipedia/pageviews-differential-private.csv"))
pageviews_daily <- read_csv(here("3-data/wikipedia/pageviews-daily.csv"))
pageviews_weekly <- read_csv(here("3-data/wikipedia/pageviews-weekly.csv"))
pageviews_total <- read_csv(here("3-data/wikipedia/total-pageviews.csv"))

# load mpox case data
cases_daily <- read_csv(cases_daily, here("3-data/mpox-cases/mpox-cases-daily.csv"))
cases_weekly <- read_csv(cases_weekly, here("3-data/mpox-cases/mpox-cases-weekly.csv"))


# Wikipedia pageview data ======================================================
# Distribution of language project views for the US
plot_df <- pageviews_total |> 
  filter(iso2 == "US") |> 
  group_by(year, month) |> 
  mutate(
    pct_pageviews_ceil = pageviews_ceil / sum(pageviews_ceil),
    total_pageviews_ceil = sum(pageviews_ceil)
    ) |> 
  ungroup() |> 
  mutate(date = as_date(glue("{year}-{month}-01"))) |> 
  select(iso2, date, project, pct_pageviews_ceil, pageviews_ceil, total_pageviews_ceil) 
project_order <- plot_df |> reframe(.by = project, total_project_views = sum(pageviews_ceil)) |> arrange(total_project_views) |> pull(project)
plot_df |> 
  mutate(project = factor(project, levels = project_order)) |> 
  ggplot(aes(x = date, y = pct_pageviews_ceil, fill = project)) +
  geom_col() + 
  scale_x_date(date_labels = "%b\n%Y") +
  scale_y_continuous(labels = label_percent()) +
  #scale_fill_discrete() +
  labs(
    title = "United States",
    subtitle = "Share of total pageviews",
    x = NULL,
    y = "% of total pageviews",
    fill = "Language project"
  ) +
  theme_minimal()
project_order2 <- plot_df |> reframe(.by = project, total_project_views = sum(pageviews_ceil)) |> arrange(total_project_views) |> head(5) |> pull(project)
library(forcats)
plot_df |>
  mutate(
    project = fct_lump_n(project, n = 4, w = pageviews_ceil),
    project = factor(project, levels = c("en.wikipedia", "fr.wikipedia", "zh.wikipedia", "es.wikipedia", "Other")),
    project = fct_rev(project)
    ) |>
  ggplot(aes(x = date, y = pct_pageviews_ceil, fill = project)) +
  geom_col() + 
  scale_x_date(date_labels = "%b\n%Y") +
  scale_y_continuous(labels = label_percent()) +
  scale_fill_brewer(type = "qual", palette = 3, direction = -1) +
  labs(
    title = "United States",
    subtitle = "Pageviews by language project",
    x = NULL,
    y = "% of total pageviews",
    fill = "Language project"
  ) +
  theme_minimal()

plot_df |> 
  filter(project == "en.wikipedia") |> 
  ggplot(aes(date, pct_pageviews_ceil)) + 
  geom_line() +
  scale_x_date(date_labels = "%b\n%Y") +
  scale_y_continuous(limits = c(0, 1), labels = label_percent()) +
  #scale_fill_discrete() +
  labs(
    title = "United States",
    subtitle = "Share of total pageviews",
    x = NULL,
    y = "% of total pageviews",
    fill = "Language project"
  ) +
  theme_minimal()

plot_df |> 
  filter(project == "en.wikipedia") |> 
  pull(pct_pageviews_ceil) |> 
  min()

plot_df |> 
  filter(project == "en.wikipedia") |> 
  pull(pct_pageviews_ceil) |> 
  max()
# ~90-95%


# Faceted plot of US mpox-related pageviews over time
pageviews |> 
  filter(country_long == "United States of America") |>
  filter(project == "en.wikipedia") |> 
  ggplot(aes(x = date, y = pageviews, color = project)) +
  geom_point(alpha = 0.75) + 
  geom_hline(yintercept = 450, color = muted("red"), linetype = "dashed") +
  scale_x_date(date_labels = "%b\n%Y") +
  facet_wrap(~page_title, scales = "free_y", nrow = 9) +
  labs(
    title = "United States of America",
    x = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# plot daily pageviews
pageviews_daily |>
  ggplot(aes(x = date, y = pageviews, color = wikidata_id)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 450, color = "red", linetype = "dashed") +
  scale_x_date(
    limits = c(min(pageviews_daily$date), max(pageviews_daily$date)),
    expand = expansion(mult = 0.05),
    date_labels = "%b %Y"
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0.02, 0.02)),
    breaks = pretty_breaks(),
    labels = comma_format()
  ) +
  labs(
    title = "Daily views of mpox-related Wikipedia pages",
    x = NULL,
    y = "Views",
    color = NULL,
    caption = "Source: Wikimedia Foundation"
  ) +
  facet_wrap(~country_long, ncol = 3, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none")

# save plot
ggsave(here("5-visualization/wiki-pageviews-daily.png"))


# plot weekly pageviews
pageviews_weekly |>
  ggplot(aes(x = date, y = pageviews, color = wikidata_id)) +
  geom_point(alpha = 0.75) +
  geom_hline(yintercept = 450, color = "red", linetype = "dashed") + # min pageviews
  scale_x_date(
    limits = c(min(pageviews_weekly$date), max(pageviews_weekly$date)),
    expand = expansion(mult = 0.05),
    date_labels = "%b %Y"
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0.02, 0.02)),
    breaks = pretty_breaks(),
    labels = comma_format()
  ) +
  labs(
    title = "Weekly pageviews of mpox-related Wikipedia pages",
    x = NULL,
    y = "Views",
    color = NULL,
    caption = "Source: Wikimedia Foundation"
  ) +
  facet_wrap(~country_long, ncol = 3, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none")

# save plot
ggsave(here("5-visualization/wiki-pageviews-weekly.png"))


# plot daily pageviews
mpox_df |>
  ggplot(aes(x = date, y = pct_pageviews, color = page_title)) +
  geom_point() +
  scale_x_date(
    limits = c(min(mpox_df$date), max(mpox_df$date)),
    expand = expansion(mult = 0.05),
    date_labels = "%b\n%Y"
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0.02, 0.02)),
    breaks = pretty_breaks(),
    labels = label_percent()
  ) +
  labs(
    title = "United States",
    subtitle = "Daily views of mpox-related Wikipedia pages",
    x = NULL,
    y = "% of monthly pageviews",
    color = NULL,
    caption = "Source: Wikimedia Foundation"
  ) +
  facet_wrap(~page_title, ncol = 3, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none")

# save plot
ggsave(here("5-visualization/wiki-pageviews-pct.png"), height = 7.75, width = 10)



# Mpox case data ===============================================================
# plot daily cases
cases_daily |> 
  reframe(cases = sum(cases), .by = date) |> 
  ggplot(aes(x = date, y = cases)) + 
  geom_col(col = "black", width = 0.9) +
  expand_limits(y = 5) +
  scale_x_date(
    limits = c(as_date("2022-01-01"), max(cases_daily$date)), 
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
cases_daily |> 
  reframe(cases_moving_avg = sum(cases_moving_avg), .by = date) |> 
  ggplot(aes(x = date, y = cases_moving_avg)) + 
  geom_col(col = "black", width = 0.5) +
  expand_limits(y = 5) +
  scale_x_date(
    limits = c(as_date("2022-01-01"), max(cases_daily$date)), 
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
cases_weekly |> 
  reframe(cases = sum(cases), .by = date) |> 
  ggplot(aes(x = date, y = cases)) + 
  geom_col(col = "black", width = 7) +
  expand_limits(y = 5) +
  scale_x_date(
    limits = c(as_date("2022-01-01"), max(cases_daily$date)), 
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


# calculate total cases by country 
cases_totals <- cases_daily |> 
  reframe(cases = sum(cases), .by = c(country, iso3)) |> 
  left_join(iso_ref, by = join_by(iso3))

# set break values
break_values <- c(0, 10, 100, 1000, 10000, Inf)
left_join(world, cases_totals, by = join_by(iso_a2 == iso2)) |> # merge geometries with case data
  tm_shape() + # create map
  tm_polygons(fill = "cases", breaks = break_values, palette = "Blues", title = "Mpox cases")
