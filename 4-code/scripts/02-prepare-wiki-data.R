# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================


# Key parameters ---------------------------------------------------------------
# define country
country_name <- "United States of America" ###

# define project
project_code <- "en.wikipedia" ### English-language Wikipedia

# define time period
start_date <- ymd("2022-01-01") ###
end_date <- ymd("2023-02-05") ###
date_sequence <- seq.Date(from = start_date, to = end_date, by = "day")


# Download historical Wikipedia Pageviews Differential Privacy data ------------
# download data
for (date in date_sequence) {
  # file path for download
  dest_path <- here(glue("data/input/wikipedia/{as_date(date)}.tsv"))

  # skip if file already exists
  if (file.exists(dest_path)) next

  # URL
  url <- glue("https://analytics.wikimedia.org/published/datasets/country_project_page_historical/{as_date(date)}.tsv")

  # try to download file
  result <- try(download.file(url, destfile = dest_path), silent = TRUE)

  # check for error
  if (inherits(result, "try-error")) {
    message("Failed to download: ", url)
    next
  }
}


# Load keywords ----------------------------------------------------------------
load(here("data/output/mpox_keywords.RData"))


# Query total monthly country-level pageviews data -----------------------------
# source: https://wikitech.wikimedia.org/wiki/Analytics/AQS/Pageviews#Pageviews_split_by_country
# context: https://wikitech.wikimedia.org/wiki/Analytics/AQS/Pageviews/Pageviews_per_project

# function to get and parse data for a given month
fetch_pageviews_for_month <- function(yyyy_mm) {
  year <- str_extract(yyyy_mm, "^\\d{4}")
  month <- str_extract(yyyy_mm, "\\d{2}$")

  url <- glue("https://wikimedia.org/api/rest_v1/metrics/pageviews/top-by-country/{project_code}/all-access/{year}/{month}")

  response <- GET(url) # API request

  if (status_code(response) == 200) {
    data <- content(response, "parsed", type = "application/json") # parse JSON
    countries_list <- data$items[[1]]$countries
    countries_df <- map_dfr(countries_list, ~ as_tibble(.x)) |>
      select(iso2 = country, pageviews_ceil = views_ceil) |>
      mutate(year = as.numeric(year), month = as.numeric(month)) # add year and month
  } else {
    return(NULL) # if fails, return NULL
  }
  return(countries_df)
}

# combine data for all months
month_sequence <- date_sequence |>
  str_remove("-\\d\\d$") |>
  unique()
pageviews_total <- map_df(month_sequence, fetch_pageviews_for_month)
pageviews_total <- pageviews_total |>
  filter(iso2 == "US") # USA


# Load historical Wikipedia Pageviews Differential Privacy data ----------------
# define file paths to data
file_paths <- list.files(
  path = "data/input/wikipedia",
  pattern = "\\.tsv$",
  full.names = TRUE
)

# function to read and wrangle data
import_pageviews_tsv <- function(file_path) {
  # extract date
  date_str <- str_extract(file_path, "\\d{4}-\\d{2}-\\d{2}")
  print(date_str)

  # load data
  df <- read_tsv(
    file_path,
    col_names = c(
      "country",
      "iso2",
      "project",
      "page_id",
      "page_title",
      "pageviews_dp"
    )
  )

  # extract relevant pages
  df <- df |>
    filter(
      country == country_name, # USA
      project == project_code, # English-language Wikipedia
      page_title %in% mpox_keywords_extended, # keywords
    ) |>
    mutate(
      date = as_date(date_str), # add date col
      page_title = str_replace_all(page_title, "_", " ") # remove underscores
    ) |>
    rename(pageviews = pageviews_dp, country_long = country)

  return(df)
}

if (!file.exists(here("data/output/pageviews_daily_usa.csv"))) { ###
  # load, clean, and combine dataframes
  pageviews <- map_dfr(file_paths, import_pageviews_tsv) # ~1 min

  # save daily pageviews data
  write_csv(pageviews, here("data/output/pageviews_daily_usa.csv")) ###
} else pageviews <- read_csv(here("data/output/pageviews_daily_usa.csv")) ###


# Prepare data -----------------------------------------------------------------
# combine pageviews for "Mpox" and "Monkeypox virus" pages
pageviews_mpox <- pageviews |>
  select(-page_id) |>
  filter(page_title %in% c("Monkeypox", "Monkeypox virus")) |>
  group_by(country_long, iso2, project, date) |>
  summarize(pageviews = sum(pageviews)) |>
  ungroup() |>
  mutate(page_title = "Monkeypox/Monkeypox virus") |>
  relocate(c(page_title, date), .before = pageviews)

# tidy pageviews data
pageviews <- pageviews |>
  select(-page_id) |>
  filter(!page_title %in% c("Monkeypox", "Monkeypox virus")) |>
  bind_rows(pageviews_mpox) |> # append combined "Monkeypox"/"Monkeypox virus" obs
  relocate(c(page_title, date), .before = pageviews) |>
  arrange(date, page_title)

# save daily pageviews data
write_csv(pageviews, here("data/output/pageviews_daily.csv")) ###


# calculate weekly pageviews
pageviews_wk <- pageviews |>
  mutate(
    date = ceiling_date(date, unit = "weeks", week_start = 3),
    year = year(date),
    month = month(date)
  ) |>
  group_by(country_long, iso2, project, page_title, year, month, date) |>
  summarize(pageviews = sum(pageviews)) |>
  ungroup() |>
  left_join(pageviews_total, by = c("iso2", "year", "month")) |>
  mutate(pageviews_pct = pageviews / pageviews_ceil * 100) |>
  select(-year, -month)

# save weekly pageviews data
write_csv(pageviews_wk, here("data/output/pageviews_weekly.csv")) ###


# Explore data -----------------------------------------------------------------
# number of mpox keywords found in pageviews data
n_mpox_pages <- mpox_keywords[mpox_keywords %in% c(pageviews$page_title, "Monkeypox", "Monkeypox virus")] |> length()
glue("{round(n_mpox_pages / length(mpox_keywords) * 100, 1)}% ({n_mpox_pages}/{length(mpox_keywords)}) of mpox keywords are present in the pageviews data.")


# plot daily pageviews
pageviews |>
  ggplot(aes(x = date, y = pageviews, color = page_title)) +
  geom_line() +
  geom_hline(yintercept = 450, color = "red", linetype = "dashed") + # min pageviews
  scale_x_date(
    limits = c(min(pageviews$date), max(pageviews$date)),
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
    title = "Daily views of mpox-related Wikipedia pages",
    subtitle = country_name,
    x = NULL,
    y = "Views",
    color = "Page title",
    caption = "Source: Wikimedia Foundation"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# save plot
ggsave(here("visualization/wiki-pageviews-daily.png"))


# plot weekly pageviews
pageviews_wk |>
  ggplot(aes(x = date, y = pageviews, color = page_title)) +
  geom_line(alpha = 0.75) +
  geom_hline(yintercept = 450, color = "red", linetype = "dashed") + # min pageviews
  scale_x_date(
    limits = c(min(pageviews_wk$date), max(pageviews_wk$date)),
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
    subtitle = country_name,
    x = NULL,
    y = "Views",
    color = "Page title",
    caption = "Source: Wikimedia Foundation"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# save plot
ggsave(here("visualization/wiki-pageviews-weekly.png"))


# Implement inclusion criteria for Wikipedia pages -----------------------------
# prepare data for comparison between "Mpox"/"Monkeypox virus" pages and all others
pageviews_pivot <- pageviews |>
  pivot_wider(
    id_cols = c(country_long, iso2, project, date),
    names_from = page_title,
    values_from = pageviews
  ) |>
  pivot_longer(
    cols = -c(country_long, iso2, project, date, `Monkeypox/Monkeypox virus`),
    names_to = "page_title",
    values_to = "pageviews"
  )

# calculate linear models for "Mpox"/"Monkeypox virus" and tidy output
pageviews_cor <- pageviews_pivot |>
  group_by(page_title) |>
  filter(!is.na(pageviews) & !is.na(`Monkeypox/Monkeypox virus`)) |> # ensure no NA values for both vars
  do(tidy(lm(pageviews ~ `Monkeypox/Monkeypox virus`, data = .))) |>
  ungroup() |>
  filter(term == "`Monkeypox/Monkeypox virus`") |>
  select(page_title, estimate, std.error, statistic, p.value)

# pages most correlated with "Mpox"/"Monkeypox virus"
page_title_cor <- pageviews_cor |> 
  arrange(-estimate) 


# plot association between "Mpox"/"Monkeypox virus" and all others
pageviews_pivot |>
  mutate(page_title = factor(page_title, levels = page_title_cor$page_title)) |> filter(!is.na(page_title)) |> 
  ggplot(aes(x = `Monkeypox/Monkeypox virus`, y = pageviews)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  facet_wrap(~page_title, scales = "fixed") +
  theme_minimal()

# save plot
ggsave(here("visualization/wiki-pageviews-page-correlation.png"))


# create list of relevant mpox-related Wikipedia pages 
mpox_page_titles <- page_title_cor |> 
  filter(
    estimate > 0,
    p.value < 0.5,
    !page_title %in% c("Viral disease", "Herd immunity", "Infection", "World Health Organization", "Clade", "Signs and symptoms") # manually remove those which appear to be spurious correlation
    ) |> 
  pull(page_title)
mpox_page_titles <- c("Monkeypox/Monkeypox virus", mpox_page_titles)


# Aggregate relevant mpox-related pageviews ------------------------------------
# calculate aggregate daily pageviews 
pageviews_agg <- pageviews |> 
  filter(page_title %in% mpox_page_titles) |> 
  group_by(country_long, iso2, project, date) |> 
  summarise(pageviews = sum(pageviews)) |> 
  ungroup()

# calculate aggregate weekly pageviews 
pageviews_agg_wk <- pageviews |> 
  filter(page_title %in% mpox_page_titles) |> 
  mutate(date = ceiling_date(date, unit = "weeks", week_start = 3)) |> 
  group_by(country_long, iso2, project, date) |> 
  summarise(pageviews = sum(pageviews)) |> 
  ungroup()


# plot daily aggregate pageviews
pageviews_agg |>
  ggplot(aes(x = date, y = pageviews)) +
  geom_line() +
  geom_hline(yintercept = 450, color = "red", linetype = "dashed") + # min pageviews
  scale_x_date(
    limits = c(min(pageviews$date), max(pageviews$date)),
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
    title = "Daily views of mpox-related Wikipedia pages",
    subtitle = country_name,
    x = NULL,
    y = "Views",
    color = "Page title",
    caption = "Source: Wikimedia Foundation"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# save plot
ggsave(here("visualization/wiki-pageviews-aggregate-daily.png"))


# plot weekly aggregate pageviews
pageviews_agg_wk |>
  ggplot(aes(x = date, y = pageviews)) +
  geom_line() +
  scale_x_date(
    limits = c(min(pageviews$date), max(pageviews$date)),
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
    title = "Weekly views of mpox-related Wikipedia pages",
    subtitle = country_name,
    x = NULL,
    y = "Views",
    color = "Page title",
    caption = "Source: Wikimedia Foundation"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# save plot
ggsave(here("visualization/wiki-pageviews-aggregate-weekly.png"))
