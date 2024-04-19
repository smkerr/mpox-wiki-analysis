library(fable)
library(feasts)
library(tsibble)


# load mpox data
mpox_df <- read_csv(here("3-data/output/mpox-data.csv"))

# Aggregate "Monkeypox" and "Monkeypox virus" pageviews
mpox_ts <- mpox_df |> 
  filter(date >= as_date("2022-05-01")) |> 
  filter(page_title %in% c("Monkeypox", "Monkeypox virus")) |> 
  select(country, iso2, iso3, date, cases, page_title, pageviews, pageviews_ceil, n_articles, n_studies) |> 
  reframe(
    country, iso2, iso3, cases, n_articles, n_studies,
    .by = date, 
    pageviews = sum(pageviews),
    pct_pageviews = pageviews / pageviews_ceil
    ) |> 
  distinct() |> # remove duplicates
  #select(date, cases, pct_pageviews, n_articles, n_studies, pageviews) |> 
  as_tsibble(key = c(country, iso2, iso3), index = date) |> # convert to tsibble
  fill_gaps() |> 
  complete(fill = list(cases = 0, pageviews = 450, n_articles = 0, n_studies = 0)) |> 
  left_join(mpox_df |> distinct(date, pageviews_ceil), join_by(date)) |> 
  fill(pageviews_ceil, .direction = "downup") |> 
  mutate(pct_pageviews = ifelse(is.na(pct_pageviews), pageviews / pageviews_ceil, pageviews)) |> 
  select(country, iso2, iso3, date, cases, pct_pageviews, n_articles, n_studies) |> 
  mutate(
    cases = zoo::rollmean(cases, k = 14, fill = 0, align = "right"),
    pct_pageviews = zoo::rollmean(pct_pageviews, k = 14, fill = 0, align = "right"),
    n_articles = zoo::rollmean(n_articles, k = 14, fill = NA, align = "right"),
    n_studies = zoo::rollmean(n_studies, k = 14, fill = NA, align = "right")
  ) |> 
  as_tsibble(key = c(country, iso2, iso3), index = date)

# Plot the series
mpox_ts |> 
  filter(date >= as_date("2022-05-01")) |> 
  pivot_longer(cols = cases:n_studies, names_to = "series", values_to = "value") |> 
  ggplot(aes(x = date, y = value, color = series)) +
  geom_line() +
  facet_wrap(~series, ncol = 1, scales = "free_y") +
  labs(title = "Time Series Plot", x = "Date", y = "Value") +
  theme_minimal()

mpox_ts |> autoplot(cases)

mpox_ts |> 
  group_by_key() |> 
  index_by(year_week = ~ yearweek(.)) |>  # weekly aggregates
  summarise(
    avg_cases = mean(cases, na.rm = TRUE),
    avg_n_articles = mean(n_articles, na.rm = TRUE)
  )

fit <- mpox_ts |> 
  model(
    ets = ETS(cases ~ trend("A")),
    arima1 = ARIMA(cases ~ pct_pageviews),
    arima2 = ARIMA(cases ~ pct_pageviews + n_articles),
    arima3 = ARIMA(cases ~ pct_pageviews + n_articles + n_studies)
  )
fit
fit |> coef()

fit |> glance()
fit |> select(arima) |> report()
fit |> augment()
fit |> accuracy() |> arrange(MASE)
fit |> forecast(h = "7 days")
fit |> forecast(h = "7 days") |> hilo(level = c(80, 95)) # conf intervals
fit |> forecast(h = "7 days") |> autoplot(mpox_ts)
fit$arima3 |> equation()

fit <- mpox_ts |> model(ARIMA(cases ~ pct_pageviews + n_articles + n_studies))
fit$ets |> equation()
fit |> accuracy() |> view()

library(fable.prophet)
fit <- mpox_ts |> model(prophet = prophet(cases ~ season("day", 4, type = "multiplicative")))
fit |> components()
fit |> forecast(h = 24) |> 
  ggplot(aes(x = date, y = .mean)) +
  geom_line() + 
  geom_smooth()

mpox_ts |> gg_season()
mpox_ts |> gg_tsdisplay()
mpox_ts |> model(ARIMA(cases ~ pct_pageviews + n_articles + n_studies)) |> gg_tsresiduals()
mpox_ts |> model(ARIMA(cases ~ pct_pageviews + n_articles + n_studies)) |> gg_arma()
dcmp <- mpox_ts |> 
  model(STL(cases ~ season(window = Inf)))
components(dcmp)
components(dcmp) |>  autoplot()
mpox_ts |> 
  features(cases, feat_stl)
accuracy(fit)

# Fit a VAR model
model_fit <- mpox_ts |> 
  filter(date >= as_date("2022-05-01")) |> 
  model(VAR(vars(cases, pct_pageviews)))

# Summary of the model
summary(model_fit)


