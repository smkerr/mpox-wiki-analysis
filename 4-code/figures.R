# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Author: Steve Kerr
# Date: April 2024
# ==============================================================================


# Setup ========================================================================
# Load packages
pacman::p_load(
  MASS, 
  dplyr,
  forcats,
  ggplot2,
  here, 
  lubridate,
  readr, 
  scales,
  spData,
  tmap
)

# Load data
# load combined data
mpox_df <- read_csv(here("3-data/output/mpox-data.csv"))

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
cases_totals <- read_csv(here("3-data/mpox-cases/cdc-mpox-cases-totals.csv"))

# load media coverage data
news_df <- read_csv(here("3-data/mpox-news/mpox-total-articles-deduplicated.csv"))

# load academic interest data
studies_df <- read_csv(here("3-data/mpox-studies/mpox-total-studies.csv"))

# ISO ref table
load(here("3-data/ref/iso_codes.RData"))


# Wikipedia Pageview Data ======================================================
## U.S. Pageviews by Mpox-related Article --------------------------------------
# Order mpox-related articles by date of peak pageviews
order_by_peak <- mpox_df |> 
  filter(date >= as_date("2022-05-01")) |> 
  filter(pageviews != 450 & pageviews != 0) |> # remove any imputed values
  group_by(page_title) |> 
  filter(sum(!is.na(pageviews)) > 5) |> # remove articles with too few observations
  slice_max(pageviews) |> 
  ungroup() |> 
  arrange(date) |> 
  pull(page_title)
  
# Create plot
mpox_df |> 
  filter(date >= as_date("2022-05-01")) |> 
  filter(pageviews != 450 & pageviews != 0) |> # remove any imputed values
  group_by(page_title) |> 
  filter(sum(!is.na(pageviews)) > 5) |> # remove articles with too few observations
  ungroup() |> 
  mutate(page_title = factor(page_title, levels = order_by_peak)) |> 
  ggplot(aes(x = date, y = pageviews, color = page_title)) +
  geom_vline(xintercept = as_date("2023-09-20"), color = "steelblue") +
  geom_segment(aes(x = as_date("2022-01-01"), y = 450, # threshold during initial period
                   xend = as_date("2023-09-20"), yend = 450), 
               linetype = "dashed", color = muted("red"), linewidth = 0.5) + 
  geom_segment(aes(x = as_date("2023-09-18"), y = 90, # threshold during later period
                   xend = as_date("2024-02-27"), yend = 90), 
               linetype = "dashed", color = muted("red")) + 
  geom_point(alpha = 0.75, size = 0.5) + 
  scale_x_date(date_labels = "%b\n%Y") +
  facet_wrap(~page_title, scales = "free_y", ncol = 2) +
  labs(
    title = "U.S. Pageviews by Mpox-related Wikipedia Article",
    subtitle = "Note: Beginning September 20, 2023, the minimum number of pageviews required for data release was lowered from 450 to 90",
    x = NULL,
    y = "Number of pageviews",
    caption = "Source: Wikimedia Foundation"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# save plot
ggsave(here("5-visualization/wiki-pageviews-by-article-USA-daily.png"))


# U.S. Percentage of Mpox-related Pageviews ------------------------------------
# TODO
# TODO: Include annotations for key events during outbreak
# TODO: Could possibly distinguish between "Monkeypox", "Monkeypox virus", and "Mpox"


## U.S. Share of Wikipedia Pageviews by Language Project -----------------------
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

# Order U.S. language projects by popularity 
project_order <- plot_df |> 
  reframe(.by = project, total_project_views = sum(pageviews_ceil)) |>
  arrange(total_project_views) |> 
  pull(project)

# Create plot 
plot_df |>
  mutate(
    project = fct_lump_n(project, n = 4, w = pageviews_ceil),
    project = factor(project, levels = c("Other", "es.wikipedia", "zh.wikipedia", 
                                         "fr.wikipedia" ,"en.wikipedia"))
    ) |>
  ggplot(aes(x = date, y = pct_pageviews_ceil, fill = project)) +
  geom_col() + 
  scale_x_date(date_labels = "%b\n%Y") +
  scale_y_continuous(labels = label_percent()) +
  scale_fill_brewer(
    type = "qual", palette = 3, direction = -1,
    breaks = c("en.wikipedia", "fr.wikipedia", "zh.wikipedia", "es.wikipedia", "Other")
    ) +
  labs(
    title = "U.S. Share of Wikipedia Pageviews by Language Project",
    x = NULL,
    y = "% of total pageviews",
    fill = "Language project",
    caption = "Source: Wikimedia Foundation"
  ) +
  theme_minimal()

# Save plot
ggsave(here("5-visualization/wiki-project-views-USA-monthly.png"), height = 7.75, width = 10)



# CDC Case Data ================================================================
## U.S. States Map -------------------------------------------------------------
break_values <- c(1, 10, 100, 1000, Inf) # set break values
us_states_map <- left_join(us_states, cases_totals, by = join_by(NAME == Location)) |> 
  #tm_shape() +
  tm_shape(crs = "EPSG:5070") + #
  tm_polygons(
    title = "Mpox cases",
    fill = "Cases", 
    breaks = break_values, 
    palette = "YlOrBr",
    labels = c("1-9", "10-99", "100-999", "1,000+")
  ) +
  tm_title(text = "U.S. Mpox Cases", height = 2) + 
  tm_title(text = "Data as of March 5, 2024", size = 1) + 
  tm_legend(bg.color = "white", position = tm_pos_in("left", "bottom")) + 
  tm_credits("Source: U.S. CDC") 
  #tm_credits("Note: Alaska (5 cases) and Hawaii (40 cases) are not shown.")
us_states_map

# Save map
tmap_save(us_states_map, filename = here("5-visualization/mpox-cases-USA-states-map.png"),
          width = 10, height = 5, dpi = 300)

# TODO: Note that Alaska (5 cases) and Hawaii (40 cases) are not pictured
cases_totals |> 
  filter(Location %in% c("Alaska", "Hawaii"))


## U.S. Daily Cases Barplot ----------------------------------------------------
cases_daily |> 
  reframe(cases = sum(cases), .by = date) |> 
  ggplot(aes(x = date, y = cases)) + 
  geom_col(width = 1) +
  expand_limits(y = 5) +
  scale_x_date(
    limits = c(as_date("2022-05-01"), max(cases_daily$date)), 
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
  scale_fill_brewer() +
  labs(
    title = "U.S. Daily Mpox Cases",
    subtitle = "Data as of March 5, 2024",
    x = "Date reported",
    y = "Cases",
    caption = "Source: U.S. CDC",
    fill = NULL
  ) +
  theme_minimal() + 
  theme(
    legend.position = "none",
    plot.margin = unit(c(0.2, 1, 0.2, 0.2), units = "cm")
  )

# Save plot
ggsave(here("5-visualization/mpox-cases-USA-daily.png"), height = 7.75, width = 10)


## U.S. Weekly Cases Barplot ---------------------------------------------------
cases_daily |> 
  mutate(date = floor_date(date, unit = "weeks")) |> 
  reframe(cases = sum(cases), .by = date) |> 
  ggplot(aes(x = date, y = cases)) + 
  geom_col(width = 7, color = "black") +
  expand_limits(y = 5) +
  scale_x_date(
    limits = c(as_date("2022-05-01"), max(cases_daily$date)), 
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
  scale_fill_brewer() +
  labs(
    title = "U.S. Weekly Mpox Cases",
    subtitle = "Data as of March 5, 2024",
    x = "Week reported",
    y = "Cases",
    caption = "Source: U.S. CDC",
    fill = NULL
  ) +
  theme_minimal() + 
  theme(
    legend.position = "none",
    plot.margin = unit(c(0.2, 1, 0.2, 0.2), units = "cm")
  )

# Save plot
ggsave(here("5-visualization/mpox-cases-USA-weekly.png"), height = 7.75, width = 10)


# Combined Data ================================================================
## U.S. Weekly Mpox Cases & Mpox Pageviews --------------------------------------------
# TODO: Consider plotting at a daily frequency for comparison
coeff <- 50000000 # value to transform scales
mpox_df |>
  filter(page_title == "Mpox") |> 
  filter(date >= as_date("2022-05-01")) |> 
  mutate(date = floor_date(date, unit = "weeks")) |> 
  reframe(
    .by = c(iso2, project, date), 
    cases = sum(cases, na.rm = TRUE), 
    pageviews = sum(pageviews, na.rm = TRUE)
    ) |> 
  mutate(year = year(date), month = month(date)) |> 
  left_join(pageviews_total, by = join_by(iso2, project, year, month)) |> 
  mutate(pct_pageviews = pageviews / pageviews_ceil) |> # express pageviews as percentage of monthly total
  ggplot(aes(x = date)) +
  geom_col(aes(y = cases / coeff, fill = "Weekly cases")) +
  geom_line(aes(y = pct_pageviews), color = "red", linewidth = 1, alpha = 0.5) +
  scale_x_date(date_labels = "%b\n%Y") +
  scale_y_continuous(
    name = "Pageviews (%)", # first axis
    sec.axis = sec_axis(~ . * coeff, name = "Cases"), # second axis
    labels = label_percent()
  ) +
  scale_fill_brewer(type = "qual", palette = 3) +
  labs(
    title = "U.S. Weekly Mpox Cases & Pageviews",
    x = NULL,
    fill = NULL,
    color = NULL,
    caption = "Source: U.S. CDC"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
p

# save plot
ggsave(here("5-visualization/mpox-cases-&-wiki-pageviews-USA-weekly.png"), height = 7.75, width = 10)


# News Coverage Data ===========================================================
## U.S. Daily Mpox-related News Articles ---------------------------------------
# TODO: Add moving average
news_df |> 
  filter(date >= as_date("2022-05-01")) |> 
  reframe(.by = date, n_articles = sum(n_articles)) |> 
  ggplot(aes(x = date, y = n_articles)) +
  geom_col() +
  scale_x_date(date_labels = "%b\n%Y") +
  labs(
    title = "Weekly number of mpox-related news articles",
    x = NULL,
    y = "Number of news articles",
    caption = "Source: GNews"
  ) +
  theme_minimal()

## U.S. Weekly Mpox-related News Articles --------------------------------------
# TODO: Add moving average
news_df |> 
  filter(date >= as_date("2022-05-01")) |> 
  mutate(date = floor_date(date, unit = "weeks")) |> 
  reframe(.by = date, n_articles = sum(n_articles)) |> 
  ggplot(aes(x = date, y = n_articles)) +
  geom_col() +
  scale_x_date(date_labels = "%b\n%Y") +
  labs(
    title = "Weekly number of mpox-related news articles",
    x = NULL,
    y = "Number of news articles",
    caption = "Source: GNews"
  ) +
  theme_minimal()

## U.S. Monthly Mpox-related News Articles -------------------------------------
# TODO: Add moving average
news_df |> 
  filter(date >= as_date("2022-05-01")) |> 
  mutate(date = floor_date(date, unit = "months")) |> 
  reframe(.by = date, n_articles = sum(n_articles)) |> 
  ggplot(aes(x = date, y = n_articles)) +
  geom_col() +
  scale_x_date(date_labels = "%b\n%Y") +
  labs(
    title = "Monthly number of mpox-related news articles",
    x = NULL,
    y = "Number of news articles",
    caption = "Source: GNews"
  ) +
  theme_minimal()


# Scientific Studies Data ======================================================
## Daily Mpox-related Scientific Studies ---------------------------------------
# TODO: Add moving average
studies_df |> 
  filter(date >= as_date("2022-01-01")) |> 
  ggplot(aes(x = date)) +
  geom_bar() +
  scale_x_date(date_labels = "%b\n%Y") +
  labs(
    title = "Mpox-related Studies",
    x = "Publication date",
    y = "Number of studies",
    caption = "Source: PubMed"
  ) +
  theme_minimal()


## Weekly Mpox-related Scientific Studies ---------------------------------------
# TODO: Add moving average
studies_df |> 
  filter(date >= as_date("2022-01-01")) |> 
  mutate(date = floor_date(date, unit = "weeks")) |> 
  ggplot(aes(x = date)) +
  geom_bar() +
  scale_x_date(date_labels = "%b\n%Y") +
  labs(
    title = "Mpox-related Studies",
    x = NULL, #"Publication date",
    y = "Number of studies",
    caption = "Source: PubMed"
  ) +
  theme_minimal()


## Monthly Mpox-related Scientific Studies -------------------------------------
# TODO: Add moving average
studies_df |> 
  filter(date >= as_date("2022-01-01")) |> 
  mutate(date = floor_date(date, unit = "months")) |> 
  ggplot(aes(x = date)) +
  geom_bar() +
  scale_x_date(date_labels = "%b\n%Y") +
  labs(
    title = "Mpox-related Studies",
    x = NULL, #"Publication date",
    y = "Number of studies",
    caption = "Source: PubMed"
  ) +
  theme_minimal()
