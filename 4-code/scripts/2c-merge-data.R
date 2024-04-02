# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================


# Setup ========================================================================
# load ISO code reference table
load(here("3-data/ref/iso_codes.RData"))

# Prepare daily data ===========================================================
# merge mpox cases and Wikipedia pageview data
mpox_df <- full_join(
  left_join(cases_df, iso_ref, by = join_by(country == country_name, iso3)), # append ISO data
  left_join(pageviews_df, iso_ref, by = join_by(country == country_name, iso2, iso3)), # append ISO data
  by = join_by(country, iso2, iso3, date)
  ) |>
  filter(!is.na(page_title)) |> 
  select(country, iso2, iso3, date, page_title, est_pageviews, pageviews_ceil, est_pct_pageviews, cases, cases_moving_avg) |> 
  complete(fill = list(est_pageviews = 0, est_pct_pageviews = 0, cases = 0, cases_moving_avg = 0))  # fill in missing zero

# TODO: Why are China, DRC, Italy excluded when missing page titles are filtered out?


# Implement inclusion criteria =================================================
mpox_df <- mpox_df |> 
  group_by(country) |> 
  filter(
    sum(cases) >= 1000, # remove countries with <1000 mpox cases
    sum(est_pageviews > 0) >= 90 # remove countries <90 days of pageview data
    ) |> 
  ungroup()

# TODO: Will need to either aggregate or disaggregate China's cases and pageviews
# Currently cases are aggregated while pageviews are disaggregated
# As of 20 March 2024: Cases shown include those in mainland China (1611), Hong Kong SAR (83), Taipei (335), and Macao (2)


# Aggregate weekly mpox-related pageviews by country ===========================
mpox_wk <- mpox_df |>
  mutate(date = ceiling_date(date, unit = "weeks", week_start = 3)) |>
  reframe(
    .by = c(country, iso2, iso3, page_title, date), 
    est_pageviews = sum(est_pageviews, na.rm = TRUE),
    est_pct_pageviews = est_pageviews / pageviews_ceil,
    cases = sum(cases, na.rm = TRUE),
    cases_moving_avg = sum(cases_moving_avg)
  ) |> 
  distinct()


# Visualize data ===============================================================
# plot daily pageviews
mpox_df |>
  ggplot(aes(x = date, y = est_pct_pageviews, fill = country)) +
  geom_col() +
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
    title = "Daily views of mpox-related Wikipedia pages",
    x = NULL,
    y = "Percentage of monthly views",
    color = NULL,
    caption = "Source: Wikimedia Foundation"
  ) +
  facet_wrap(~country, ncol = 3, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none")

# save plot
ggsave(here("5-visualization/wiki-pageviews-daily-pct.png"))


# plot weekly pageviews
mpox_wk |>
  ggplot(aes(x = date, y = est_pct_pageviews, fill = country)) +
  geom_col(alpha = 0.75) +
  scale_x_date(
    limits = c(min(mpox_wk$date), max(mpox_wk$date)),
    expand = expansion(mult = 0.05),
    date_labels = "%b\n%Y"
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
  facet_wrap(~country, ncol = 3, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none")

# save plot
ggsave(here("5-visualization/wiki-pageviews-weekly-pct.png"))
