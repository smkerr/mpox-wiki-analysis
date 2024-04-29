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
  glue,
  here, 
  lubridate,
  readr, 
  scales,
  slider,
  spData,
  tmap,
  install = FALSE
)

# Load data
# load combined data
mpox_df <- read_csv(here("3-data/output/mpox-data.csv"))

# load pageviews data
pageviews_total <- read_csv(here("3-data/wikipedia/project-views-monthly.csv"))

# load mpox case data
cases_daily <- read_csv(here("3-data/mpox-cases/mpox-cases-daily.csv"))
cases_totals <- read_csv(here("3-data/mpox-cases/cdc-mpox-cases-totals.csv"))

# load media coverage data
news_df <- read_csv(here("3-data/mpox-news/mpox-total-articles-deduplicated.csv"))

# load academic interest data
studies_df <- read_csv(here("3-data/mpox-studies/mpox-total-studies.csv"))

# Load lag analysis results
lag_results <- read_csv(here(glue("3-data/output/lag-analysis/lag-analysis-results.csv")))

# ISO ref table
load(here("3-data/ref/iso_codes.RData"))


# Wikipedia Pageview Data ======================================================
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
    project = case_when(
      project == "Other" ~ "Other", 
      project == "es.wikipedia" ~ "Spanish", 
      project == "zh.wikipedia" ~ "Chinese", 
      project == "fr.wikipedia" ~  "French",
      project == "en.wikipedia" ~ "English"
    ),
    project = factor(project, levels = c("Other", "Spanish", "Chinese", 
                                         "French" ,"English"))
  ) |>
  ggplot(aes(x = date, y = pct_pageviews_ceil, fill = project)) +
  geom_col() + 
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b\n%Y",
    expand = expansion()
  ) + 
  scale_y_continuous(labels = label_percent(), breaks = pretty_breaks(n = 10)) +
  scale_fill_brewer(
    type = "qual", palette = 3, direction = -1,
    breaks = c("English", "French", "Chinese", "Spanish", "Other")
  ) +
  labs(
    title = "U.S. Wikipedia Pageviews by Language",
    x = NULL, 
    y = "% of total pageviews",
    fill = "Language",
    caption = "Source: Wikimedia Foundation"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

# Save plot
ggsave(here("6-figures/pageviews-by-language.png"), 
       height = 7.75, width = 12, dpi = 600)


## U.S. Percentage of Mpox-related Pageviews ------------------------------------
mpox_df |> 
  filter(page_title == "Mpox") |> 
  filter(date >= as_date("2022-05-01")) |> 
  mutate(date = floor_date(date, unit = "weeks")) |> 
  reframe(pageviews = sum(pageviews), .by = date) |> 
  ggplot(aes(date, pageviews)) + 
  geom_col(width = 7, color = "black") +
  scale_x_date(
    limits = c(as_date("2022-0-01"), max(cases_daily$date)),
    date_breaks = "1 month",
    breaks = seq.Date(from = as_date("2022-05-15"), to = max(cases_daily$date), by = "3 months"), 
    date_labels = "%b\n%Y" 
  ) + 
  scale_y_continuous(
    limits = c(0, 200000), 
    breaks = pretty_breaks(n = 14),
    labels = comma_format()
  ) +
  labs(
    title = "U.S. Pageviews for Mpox-specific Wikipedia Articles",
    x = "Week",
    y = "Pageviews",
    caption = "Source: Wikimedia Foundation\nNote: Mpox-specific pageviews comprise those for 'Mpox' and 'Monkeypox virus' English-language articles"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text.x = element_text(hjust = 0.5)  
    )

# Save plot
ggsave(here("6-figures/pageviews-mpox-specific.png"), 
       height = 7.75, width = 12, dpi = 600)



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
  geom_segment(aes(x = as_date("2022-05-01"), y = 450, # threshold during initial period
                   xend = as_date("2023-09-19"), yend = 450), 
               linetype = "dashed", color = muted("red"), linewidth = 0.25) + 
  geom_segment(aes(x = as_date("2023-09-20"), y = 90, # threshold during later period
                   xend = as_date("2024-02-27"), yend = 90), 
               linetype = "dashed", color = muted("red"), linewidth = 0.25) + 
  geom_vline(xintercept = as_date("2023-09-20"), color = "steelblue") +
  geom_point(alpha = 0.75, size = 0.5) + 
  scale_x_date(
    limits = c(as_date("2022-05-01"), as_date("2024-02-27")), 
    expand = expansion(),
    date_breaks = "3 months",
    breaks = seq.Date(from = as_date("2022-05-01"), to = as_date("2024-02-27"), by = "3 months"), 
    date_labels = "%b\n%Y" 
  ) + 
  facet_wrap(~page_title, scales = "free_y", ncol = 2) +
  labs(
    title = "U.S. Pageviews by Mpox-related Wikipedia Article",
    x = NULL,
    y = "Pageviews",
    caption = "Source: Wikimedia Foundation\nNote: Beginning September 20, 2023, the minimum number of pageviews required for data release was lowered from 450 to 90"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
    )

# save plot
ggsave(here("6-figures/pageviews-mpox-related.png"), width = 10, height = 12)


# CDC Case Data ================================================================
## U.S. States Map -------------------------------------------------------------
break_values <- c(1, 10, 100, 1000, Inf) # set break values
us_states_map <- left_join(us_states, cases_totals, by = join_by(NAME == Location)) |> 
  tm_shape(crs = "EPSG:5070") +
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
tmap_save(us_states_map, filename = here("6-figures/cases-USA-map.png"),
          width = 10, height = 7, dpi = 300)


## U.S. Weekly Cases -----------------------------------------------------------
cases_daily |> 
  mutate(date = floor_date(date, unit = "weeks")) |> 
  reframe(cases = sum(cases), .by = date) |> 
  ggplot(aes(x = date, y = cases)) + 
  geom_col(width = 7, color = "black") +
  expand_limits(y = 5) +
  scale_x_date(
    limits = c(as_date("2022-01-01"), max(cases_daily$date)), 
    expand = expansion(mult = 0.05),
    date_breaks = "1 month",
    breaks = seq.Date(from = as_date("2022-05-01"), to = max(cases_daily$date), by = "3 months"), 
    date_labels = "%b\n%Y" 
  ) + 
  scale_y_continuous(
    limits = c(0, 3500), 
    expand = expansion(mult = c(0.02, 0.02)), 
    breaks = pretty_breaks(n = 6),
    labels = comma_format()
  ) +
  scale_fill_brewer() +
  labs(
    title = "Epidemic Curve",
    subtitle = "Data as of March 5, 2024",
    x = "Week reported",
    y = "Cases",
    caption = "Source: U.S. CDC",
    fill = NULL
  ) +
  theme_minimal() + 
  theme(
    legend.position = "none",
    plot.margin = unit(c(0.2, 1, 0.2, 0.2), units = "cm"),
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

# Save plot
ggsave(here("6-figures/cases.png"), height = 7.75, width = 10)


# Combined Data ================================================================
## U.S. Weekly Mpox Cases & Mpox Pageviews -------------------------------------
coeff <- 35000000 # value to transform scales
plot_df <- left_join(
  # calculate 7-day rolling averages for pageviews 
  mpox_df |> 
    filter(sum(pct_pageviews > 0, na.rm = TRUE) > 1) |> # remove articles with zero pageviews
    group_by(country, iso2, iso3, project, wikidata_id, page_id, page_title) |> 
    mutate(
      #pageviews = ifelse(is.na(pageviews), 0, pageviews), # set missing values equal to lower bound threshold
      pct_pageviews = pageviews / pageviews_ceil,
      roll_pct_pageviews = slide_dbl(pct_pageviews, ~mean(.x, na.rm = TRUE), .before = 3, .after = 3)
    ) |> 
    ungroup() |> 
    mutate(across(everything(), ~replace(., is.nan(.), NA))) |> 
    select(-cases:-n_studies),
  # calculate 7-day rolling averages for cases, news articles, & scientific studies
  mpox_df |> 
    distinct(date, cases, n_articles, n_studies) |> 
    mutate(roll_cases = slide_dbl(cases, .f = ~mean(.x, na.rm = TRUE), .before = 3, .after = 3)) |> 
    select(date, starts_with("roll")),
  by = join_by(date)
)

vline_dates <- c(as_date("2022-05-20"),
                 as_date("2022-07-23"),
                 as_date("2022-11-28"))
labels <- c("U.S. reports first case",
            "WHO declares PHEIC",
            'Monkeypox renamed "mpox"')



plot_df |>
  filter(page_title == "Mpox") |> 
  filter(date >= as_date("2022-05-01")) |> 
  ggplot(aes(x = date)) +
  geom_col(aes(y = roll_cases / coeff, fill = "Cases")) +
  geom_vline(xintercept = vline_dates) + 
  geom_text(data = data.frame(date = vline_dates, label = labels),
            aes(x = date, y = Inf, label = label), angle = 90, vjust = -1, hjust = 1.05, size = 3) +
  geom_line(aes(y = roll_pct_pageviews, color = "Pageviews (%)"), linewidth = 1) +
  scale_x_date(
    limits = c(as_date("2022-0-01"), max(cases_daily$date)), 
    date_breaks = "1 month",
    breaks = seq.Date(from = as_date("2022-05-01"), to = max(cases_daily$date), by = "3 months"), 
    date_labels = "%b\n%Y" ,
    expand = expansion()
  ) + 
  scale_y_continuous(
    limits = c(0, 0.000014), 
    name = "7-day average pageviews (%)", # first axis
    sec.axis = sec_axis(~ . * coeff, name = "7-day average cases", breaks = pretty_breaks(12)), # second axis
    breaks = pretty_breaks(6),
    labels = label_percent(),
    expand = expansion()
  ) +
  scale_fill_brewer(type = "qual", palette = 3) +
  labs(
    title = "U.S. Mpox-specific Wikipedia Pageviews",
    subtitle = "Note: Pageviews are expressed as a percentage of total monthly U.S. English-language pageviews",
    x = NULL,
    fill = NULL,
    color = NULL,
    caption = "Source: U.S. CDC"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
    )

# save plot
ggsave(here("6-figures/cases-&-pageviews-rolling-avg.png"), height = 7.75, width = 10)


# News Coverage Data ===========================================================
## U.S. Weekly Mpox-related News Articles --------------------------------------
news_df |> 
  filter(date >= as_date("2022-05-01")) |> 
  mutate(date = floor_date(date, unit = "weeks")) |> 
  reframe(.by = date, n_articles = sum(n_articles)) |> 
  ggplot(aes(x = date, y = n_articles)) +
  geom_col(width = 7, color = "black") +
  scale_x_date(
    limits = c(as_date("2022-0-01"), max(cases_daily$date)), 
    date_breaks = "1 month",
    breaks = seq.Date(from = as_date("2022-05-01"), to = max(cases_daily$date), by = "3 months"), 
    date_labels = "%b\n%Y" ,
    expand = expansion()
  ) + 
  scale_y_continuous(limits = c(0, 290), breaks = pretty_breaks(10)) +
  labs(
    title = "U.S. Media Coverage of Mpox",
    x = "Week",
    y = "News articles",
    caption = "Source: GNews"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

# save plot
ggsave(here("6-figures/mpox-news.png"), height = 7.75, width = 10)


# Scientific Studies Data ======================================================
## Weekly Mpox-related Scientific Studies ---------------------------------------
studies_df |> 
  filter(date >= as_date("2022-01-01")) |> 
  mutate(date = floor_date(date, unit = "weeks")) |> 
  ggplot(aes(x = date)) +
  geom_bar(width = 7, color = "black") +
  scale_x_date(
    limits = c(as_date("2022-0-01"), max(cases_daily$date)), 
    date_breaks = "1 month",
    breaks = seq.Date(from = as_date("2022-05-01"), to = max(cases_daily$date), by = "3 months"), 
    date_labels = "%b\n%Y" ,
    expand = expansion()
  ) + 
  scale_y_continuous(limits = c(0, 100), breaks = pretty_breaks(5)) +
  labs(
    title = "Mpox-related Scientific Publications",
    x = "Week of publication",
    y = "Publications",
    caption = "Source: PubMed"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

# Save plot
ggsave(here("6-figures/mpox-studies.png"), height = 7.75, width = 10)



# Lag Analysis =================================================================
## Visualize coefficient estimates ---------------------------------------------
# Order articles by average lag value of max coefficient
order_by_coefficients <- lag_results |> 
  reframe(.by = page_title, min_estimate = min(estimate, na.rm = TRUE)) |> 
  arrange(min_estimate) |> 
  pull(page_title)

# Plot heatmap of Spearman coefficients
lag_results |>
  mutate(page_title = factor(page_title, levels = order_by_coefficients)) |> 
  ggplot(aes(x = lag, y = page_title, fill = estimate, color = page_title)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#91bfdb", mid = "#ffffbf", high = "#fc8d59",  midpoint = 0, limits = c(-1, 1)) +
  scale_x_continuous(n.breaks = 15) +
  labs(
    title = "Time lag correlation of Wikipedia pageviews and mpox cases",
    x = "Time lag [days]",
    y = NULL,
    fill = "Spearman \ncorrelation \ncoefficient"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )


# Save image
ggsave(filename = here("6-figures/spearman-correlation-heatmap.png"), width = 10, height = 8, dpi = 300)


# Plot barplot of Spearman coefficients
lag_results |> 
  mutate(page_title = factor(page_title, levels = order_by_coefficients)) |> 
  ggplot(aes(x = lag, y = estimate, fill = estimate)) +
  geom_bar(stat = "identity") +
  facet_wrap(~page_title, ncol = 4) +
  scale_fill_gradient2(
    low = "#91bfdb",
    mid = "#ffffbf", 
    high = "#fc8d59", 
    midpoint = 0, 
    limits = c(-1, 1)
  ) + 
  scale_x_continuous(n.breaks = 15) +
  theme_minimal() +
  labs(
    title = "Time-lag correlations of mpox-related Wikipedia pageviews and mpox cases",
    x = "Time lag [days]",
    y = "Spearman correlation coefficient",
    fill = "Estimate"
  )

# Save image
ggsave(filename = here("6-figures/spearman-correlation-barplot.png"), width = 10, height = 8, dpi = 300)


## Visualize p-values ----------------------------------------------------------
# Plot heatmap of p-values
lag_results |> 
  mutate(
    signif = case_when(
      p.value < 0.001 ~ "<0.001", 
      p.value < 0.01 ~ "<0.01",
      p.value < 0.05 ~ "<0.05",
      TRUE ~ "Not signif."),
    signif = factor(signif, levels = c("Not signif.", "<0.05", "<0.01", "<0.001")),
    page_title = factor(page_title, levels = order_by_coefficients)
  ) |> 
  ggplot(aes(x = lag, y = page_title, fill = signif)) +
  geom_tile(color = "white") +
  scale_fill_brewer(
    type = "seq",
    palette = 4 
  ) +
  scale_x_continuous(n.breaks = 15) +
  labs(
    title = "Significance of time lag correlation of Wikipedia pageviews and mpox cases",
    x = "Time lag [days]",
    y = NULL,
    fill = "Significance\nlevel"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

# Save image
ggsave(filename = here("6-figures/spearman-pvalues-heatmap.png"), width = 10, height = 8, dpi = 300)
