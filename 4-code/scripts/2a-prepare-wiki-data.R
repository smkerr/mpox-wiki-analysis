# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================


# Key parameters ===============================================================
# define time period
start_date <- ymd("2022-01-01") ### try out with first year of outbreak
end_date <- ymd("2022-12-31") ###
date_sequence <- seq.Date(from = start_date, to = end_date, by = "day")

# define mpox-specific pages
mpox_pages  <- c("Mpox", "Monkeypox", "Monkeypox virus")

# # load relevant mpox pages
# load(here("3-data/output/mpox_pages_extended.RData"))
# mpox_pages_extended

# load ISO reference table
load(here("3-data/ref/iso_codes.RData"))


# Get page titles for other languages ==========================================
# write function to query page titles for other languages
get_alt_page_titles <- function(mpox_page) {
  url <- glue("https://en.wikipedia.org/w/api.php?action=query&titles={mpox_page}&prop=langlinks&lllimit=max&format=json") # endpoint
  
  response <- GET(url) # API request
  
  if (status_code(response) == 200) {
    data <- content(response, "parsed", type = "application/json") # parse JSON
    lang_list <- data$query$pages[[1]]$langlinks
    lang_df <- map_dfr(lang_list, ~ as_tibble(.x)) |> 
      mutate(page_title_en = mpox_page)
  } else {
    return(NULL) # if fails, return NULL
  }
  return(lang_df)
}

# combine all alternate page titles
page_title_df <- map_df(str_replace_all(mpox_pages, " ", "_"), get_alt_page_titles) |>
  rename(page_title = `*`) |> 
  bind_rows(tibble( # append mpox-related pages for English-language Wikipedia
    lang = rep("en", length(mpox_pages)),
    page_title = mpox_pages
    ))


# Get daily pageviews by project ===============================================
# write function to query pageviews for a given project and page title
get_daily_pageviews <- function(project_code, page_name) {
  url <- glue("https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/{paste0(project_code, '.wikipedia')}/all-access/user/{str_replace_all(page_name, ' ', '_')}/daily/{str_remove_all(start_date, '-')}/{str_remove_all(end_date, '-')}") # endpoint
  
  response <- GET(url) # API request
  
  if (status_code(response) == 200) {
    data <- content(response, "parsed", type = "application/json") # parse JSON
    pageviews_df <- map_dfr(data$items, ~as_tibble(.x)) |> 
      mutate(date = as_date(ymd_h(timestamp))) |> # timestamp to date type
      select(project, page_title = article, date, pageviews = views) |> 
      arrange(project, date, page_title)
  } else {
    return(NULL) # if fails, return NULL
  }
  write_csv(pageviews_df, here(glue("3-data/wikipedia/pageviews/pageviews-{project_code}.csv")))
}

# combine pageview data for all language projects 
if (!file.exists(here("3-data/wikipedia/pageviews.csv"))) {
  map2_df(page_title_df$lang, page_title_df$page_title, get_daily_pageviews)
  file_paths <- list.files(here("3-data/wikipedia/pageviews"), full.names = TRUE)
  pageviews <- map_dfr(file_paths, read_csv)
  write_csv(pageviews, here("3-data/wikipedia/pageviews.csv"))
} else {
  pageviews <- read_csv(here("3-data/wikipedia/pageviews.csv"))
}

# TODO: Need to take into account that many of these pages were created after the 
# outbreak started. Possibly exclude articles less than X days old when implementing
# inclusion criteria...


# Get monthly project views by country =========================================
# write function to query and parse data for a given project and month
get_monthly_project_views <- function(project_code, yyyy_mm) {
  year <- str_extract(yyyy_mm, "^\\d{4}")
  month <- str_extract(yyyy_mm, "\\d{2}$")
  
  url <- glue("https://wikimedia.org/api/rest_v1/metrics/pageviews/top-by-country/{paste0(project_code, '.wikipedia')}/all-access/{year}/{month}")
  
  response <- GET(url) # API request
  
  if (status_code(response) == 200) {
    data <- content(response, "parsed", type = "application/json") # parse JSON
    countries_list <- data$items[[1]]$countries
    countries_df <- map_dfr(countries_list, ~ as_tibble(.x)) |>
      mutate(year = as.numeric(year), month = as.numeric(month), project = paste0(project_code, ".wikipedia")) |> 
      select(project, iso2 = country, year, month, pageviews_ceil = views_ceil) |> 
      arrange(project, year, month, -pageviews_ceil)
  } else {
    return(NULL) # if fails, return NULL
  }
  return(countries_df)
}

# combine & save monthly project views
if (!file.exists(here("3-data/wikipedia/project-views-by-country.csv"))) {
  month_sequence <- date_sequence |> str_remove("-\\d\\d$") |> unique()
  combination_df <- crossing(project_code = page_title_df$lang, month = month_sequence)
  country_project_views <- map2_df(combination_df$project_code, combination_df$month, get_monthly_project_views)
  write_csv(country_project_views, here("3-data/wikipedia/project-views-by-country.csv"))
} else {
  country_project_views <- read_csv(here("3-data/wikipedia/project-views-by-country.csv"))
}


# Prepare data =================================================================
## Prepare pageviews -----------------------------------------------------------
# aggregate mpox-related pageviews by project
pageviews <- pageviews |>
  reframe(pageviews = sum(pageviews, na.rm = TRUE), .by = c(project, date)) |>
  mutate(page_title = "Mpox/Monkeypox virus") |> ### TODO: Need to adapt this if more types of pages are included
  relocate(page_title, .after = project) |>
  arrange(project, page_title, date)


## Prepare project views by country --------------------------------------------
# create df with complete date sequence
complete_df <- tibble(
  date = date_sequence,
  year = year(date),
  month = month(date)
)

# remove missing data
country_project_views_clean <- country_project_views |> 
  filter(!is.na(iso2), iso2 != "--")  

# calculate total monthly project view for each country 
country_project_views_total <- country_project_views_clean |> 
  reframe(.by = c(iso2, year, month), pageviews_ceil = sum(pageviews_ceil)) |> 
  right_join(complete_df, by = join_by(year, month), relationship = "many-to-many") |> 
  select(-year, -month) |> 
  relocate(date, .after = iso2) |> 
  arrange(iso2, date)

# calculate share of monthly project views for each country
country_project_views_share <- country_project_views_clean |> 
  reframe(.by = c(project, year, month), iso2, pageviews_ceil, pct_pageviews_ceil = pageviews_ceil / sum(pageviews_ceil)) |> 
  right_join(complete_df, by = join_by(year, month), relationship = "many-to-many") |> 
  select(-year, -month) |> 
  relocate(date, .after = project) |> 
  arrange(project, date)

# estimate mpox pageviews by country  
pageviews_df <- country_project_views_share |> 
  left_join(pageviews, by = join_by(project, date)) |> 
  filter(!is.na(page_title), !is.na(pageviews)) |> # remove missing data
  mutate(est_pageviews = round(pct_pageviews_ceil * pageviews)) |> 
  reframe(.by = c(iso2, page_title, date), est_pageviews = sum(est_pageviews)) |> 
  arrange(iso2, page_title, date) |> 
  left_join(iso_ref, by = join_by(iso2)) |> 
  relocate(c(country = country_name, iso2, iso3), .before = page_title)


# Normalize daily pageviews ====================================================
# express as a percentage of total monthly pageviews
pageviews_df <- pageviews_df |> 
  left_join(country_project_views_total, by = join_by(iso2, date)) |> 
  mutate(est_pct_pageviews = est_pageviews / pageviews_ceil)


# Aggregate weekly mpox-related pageviews by country ===========================
pageviews_wk <- pageviews_df |>
  mutate(date = ceiling_date(date, unit = "weeks", week_start = 3)) |>
  reframe(
    .by = c(country, iso2, iso3, page_title, date), 
    pageviews_ceil,
    est_pageviews = sum(est_pageviews),
    est_pct_pageviews = est_pageviews / pageviews_ceil
    ) |>
  relocate(date, .before = est_pageviews)

# TODO: Where are the NAs in the weekly data coming from?
pageviews_df |> 
  filter(is.na(est_pct_pageviews))

pageviews_wk |> 
  filter(is.na(est_pct_pageviews))


## Implement inclusion criteria ------------------------------------------------
# TODO: consider whether this step is even necessary .... 
included_countries <- pageviews_df |>
  reframe(est_pageviews = max(est_pageviews), .by = country) |>
  arrange(-est_pageviews) |> 
  head(21) |> 
  pull(country)
pageviews_df <- pageviews_df |> filter(country %in% included_countries)
pageviews_wk <- pageviews_wk |> filter(country %in% included_countries)


# Visualize data ===============================================================
# plot daily pageviews
pageviews_df |>
  mutate(country = factor(country, levels = included_countries)) |> 
  ggplot(aes(x = date, y = est_pct_pageviews, fill = country)) +
  geom_col() +
  scale_x_date(
    limits = c(min(pageviews_df$date), max(pageviews_df$date)),
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
pageviews_wk |>
  mutate(country = factor(country, levels = included_countries)) |> 
  ggplot(aes(x = date, y = est_pct_pageviews, fill = country)) +
  geom_col(alpha = 0.75) +
  scale_x_date(
    limits = c(min(pageviews_wk$date), max(pageviews_wk$date)),
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
  facet_wrap(~country, ncol = 3, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none")

# save plot
ggsave(here("5-visualization/wiki-pageviews-weekly-pct.png"))
