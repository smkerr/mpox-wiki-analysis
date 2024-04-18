# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================


# Load data ====================================================================
# load combined data
mpox_df # <- 

# load pageviews data
pageviews <- read_csv(here("3-data/wikipedia/pageviews-differential-private.csv"))
pageviews_daily <- read_csv(here("3-data/wikipedia/pageviews-daily.csv"))
pageviews_weekly <- read_csv(here("3-data/wikipedia/pageviews-weekly.csv"))
pageviews_total <- read_csv(here("3-data/wikipedia/total-pageviews.csv"))

# load mpox case data
## WHO data
cases_country_df <- read_csv(here("3-data/mpox-cases/mpox-cases-countries.csv"))
cases_region_df <- read_csv(here("3-data/mpox-cases/mpox-cases-regions.csv"))
## CDC data
cases_daily <- read_csv(here("3-data/mpox-cases/mpox-cases-daily.csv"))
cases_weekly <- read_csv(here("3-data/mpox-cases/mpox-cases-weekly.csv"))

# load media coverage data
news_df <- read_csv(here("3-data/mpox-news/mpox-total-articles.csv"))

# load academic interest data
studies_df <- read_csv(here("3-data/mpox-studies/mpox-total-studies.csv"))

# ISO ref table
load(here("3-data/ref/iso_codes.RData"))


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

# English Wikipedia comprises ~90-95% of US Wikipedia views 
plot_df |> 
  filter(project == "en.wikipedia") |> 
  pull(pct_pageviews_ceil) |> 
  range()


# Faceted plot of US mpox-related pageviews over time
pageviews |> 
  group_by(page_title) |> 
  filter(sum(!is.na(pageviews)) > 5) |> # remove articles with too few observations
  ungroup() |> 
  filter(country_long == "United States of America") |>
  filter(project == "en.wikipedia") |> 
  ggplot(aes(x = date, y = pageviews, color = project)) +
  geom_point(alpha = 0.75) + 
  geom_hline(yintercept = 450, color = muted("red"), linetype = "dashed") +
  scale_x_date(date_labels = "%b\n%Y") +
  facet_wrap(~page_title, scales = "free_y", ncol = 2) +
  labs(
    title = "United States of America",
    x = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# plot daily pageviews
pageviews_daily |>
  filter(iso2 == "US") |> 
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
  filter(iso2 == "US") |> 
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
  reframe(cases = sum(cases), .by = date) |> 
  ggplot(aes(x = date, y = cases)) + 
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
# order regions by total cases
cases_region_ordered <- cases_region_df |> 
  reframe(.by = region, total_cases = sum(cases)) |> 
  arrange(-total_cases)
cases_region_df <- cases_region_df |> 
  mutate(region = factor(region, levels = cases_region_ordered$region))
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


# Map of total cases
## total country-level case data 
cases_total <- cases_daily |> 
  reframe(
    .by = c(country, iso3),
    cases = sum(cases, na.rm = TRUE)
  )
break_values <- c(1, 10, 100, 1000, 10000, Inf) # set break values
tm <- cases_total |> 
  #left_join(iso_ref, by = join_by(iso3)) |> # get ISO 2 codes
  select(country, iso3, cases) |> 
  right_join(World, by = join_by(iso3 == iso_a3)) |>  # get geometries
  #filter(iso3 != "ATA") |> # remove Antartica to improve readability
  st_as_sf() |> 
  tm_shape(crs = "ESRI:53030") + # create map
  tm_layout(bg.color = "#d8f9ff") +
  tm_polygons(
    title = "Mpox cases",
    fill = "cases", 
    breaks = break_values, 
    palette = "YlOrBr",
    textNA = "None reported",
    colorNA = "white",
    labels = c("1-9", "10-99", "100-999", "1,000-9,999", "10,000+")
    ) +
  tm_legend(bg.color = "white", position = tm_pos_in("left", "bottom"))
tm

# Save map
tmap_save(tm, here("5-visualization/mpox-cases-total-world-map-v2.png"))



# Combined mpox data ===========================================================
# plot US mpox-related pageviews and mpox cases
# TODO: Either find a way to automatically scale the coefficient based on the max values of the particular page_title OR just sum all page_titles' pageviews together OR only plot mpox-related articles 
coeff <- 100000000 # value to transform scales
p <- mpox_df |>
  ggplot(aes(x = date)) +
  geom_col(aes(y = cases / coeff, fill = "Weekly confirmed cases")) +
  geom_line(aes(y = pct_pageviews)) +
  facet_wrap(~country) +
  scale_x_date(date_labels = "%b %Y") +
  scale_y_continuous(
    name = "Mpox-related pageviews (%)", # first axis
    sec.axis = sec_axis(~ . * coeff, name = "Weekly confirmed cases"), # second axis
    labels = scales::label_percent()
  ) +
  scale_fill_brewer(type = "qual", palette = 3) +
  facet_wrap(~page_title, scales = "free_y") +
  labs(
    x = NULL,
    fill = NULL,
    color = NULL,
    caption = "Source: World Health Organization"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
p

# save plot
ggsave(here("5-visualization/mpox-cases-&-wiki-pageviews.png"), height = 7.75, width = 10)


# Media coverage ===============================================================
# plot daily number of news articles
news_df |> 
  reframe(.by = date, n_articles = sum(n_articles)) |> 
  ggplot(aes(x = date, y = n_articles)) +
  geom_col() +
  theme_minimal()

# plot daily number of news articles by search term
news_df |> 
  ggplot(aes(x = date, y = n_articles)) +
  geom_col() +
  facet_wrap(~search_term, ncol = 1) + 
  theme_minimal()

# plot weekly number of news articles
news_df |> 
  mutate(date = floor_date(date, unit = "weeks")) |> 
  reframe(.by = date, n_articles = sum(n_articles)) |> 
  ggplot(aes(x = date, y = n_articles)) +
  geom_col() +
  scale_x_date(date_labels = "%b\n%Y") +
  labs(
    title = "Weekly number of mpox-related news articles",
    x = NULL
  ) +
  theme_minimal()

# plot weekly number of news articles by search term
news_df |> 
  mutate(date = floor_date(date, unit = "weeks")) |> 
  ggplot(aes(x = date, y = n_articles)) +
  geom_col() +
  facet_wrap(~search_term, ncol = 1) +
  scale_x_date(date_labels = "%b\n%Y") +
  labs(
    title = "Weekly number of mpox-related news articles",
    x = NULL
  ) +
  theme_minimal()

# plot monthly number of news articles
news_df |> 
  mutate(date = floor_date(date, unit = "months")) |> 
  reframe(.by = date, n_articles = sum(n_articles)) |> 
  ggplot(aes(x = date, y = n_articles)) +
  geom_col() + 
  scale_x_date(date_labels = "%b\n%Y") +
  labs(
    title = "Monthly number of mpox-related scientific publications",
    x = NULL
  ) +
  theme_minimal()

# plot monthly number of news articles by search term
news_df |> 
  mutate(date = floor_date(date, unit = "months")) |> 
  ggplot(aes(x = date, y = n_articles)) +
  geom_col() + 
  scale_x_date(date_labels = "%b\n%Y") +
  labs(
    title = "Monthly number of mpox-related scientific publications",
    x = NULL
  ) +
  facet_wrap(~search_term, ncol = 1) +
  theme_minimal()



# Academic studies =============================================================
# plot daily number of published studies
studies_df |> 
  ggplot(aes(x = date)) +
  geom_bar() +
  theme_minimal()

# plot weekly number of published studies
studies_df |> 
  ggplot(aes(x = date)) +
  geom_bar() + 
  scale_x_date(date_labels = "%b\n%Y") +
  labs(
    title = "Weekly number of mpox-related scientific publications",
    x = NULL
  ) +
  theme_minimal()

# plot monthly number of published studies
studies_df |> 
  mutate(date = floor_date(date, unit = "months")) |> 
  ggplot(aes(x = date)) +
  geom_bar() + 
  scale_x_date(date_labels = "%b\n%Y") +
  labs(
    title = "Monthly number of mpox-related scientific publications",
    x = NULL
  ) +
  theme_minimal()
