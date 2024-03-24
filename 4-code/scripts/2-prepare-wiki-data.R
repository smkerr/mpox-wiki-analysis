# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================


# Key parameters ---------------------------------------------------------------
## define project
project_code <- "en.wikipedia" ### English-language Wikipedia

## define time period
start_date <- ymd("2022-01-01") ### corresponds with period for which we have detailed data
end_date <- ymd("2023-02-05") ###
date_sequence <- seq.Date(from = start_date, to = end_date, by = "day")

# define keywords
mpox_keywords <- c("Mpox", "Monkeypox", "Monkeypox virus")


# Query Daily Pageview data ----------------------------------------------------
if (!file.exists(here("3-data/wikipedia/pageviews-en.csv"))) { ###
  # query page views
  pageviews <- wx_page_views(
    project = project_code,
    page_name = mpox_keywords,
    access_method = "all", # desktop + mobile 
    agent_type = "user", # human users
    granularity = "daily", 
    start_date = str_replace_all(start_date, "-", ""), 
    end_date = str_replace_all(end_date, "-", ""),
    include_redirects = TRUE
  )
  
  # save daily pageviews data
  write_csv(pageviews, here("3-data/wikipedia/pageviews-en.csv")) ###
} else pageviews <- read_csv(here("3-data/wikipedia/pageviews-en.csv")) ###


# Download Differential-Private Daily Pageview data ----------------------------
for (date in date_sequence) {
  # file path for download
  dest_path <- here(glue("3-data/wikipedia/pageviews-differential-private/{as_date(date)}.tsv"))
  
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


# Load Differential-Private Daily Pageview data --------------------------------
# define file paths to data
file_paths <- list.files(
  path = "3-data/wikipedia/pageviews-differential-private",
  pattern = "\\.tsv$",
  full.names = TRUE
)

# function to read and wrangle data
import_pageviews_tsv <- function(file_path) {
  # extract date
  date_str <- str_extract(file_path, "\\d{4}-\\d{2}-\\d{2}")
  
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
      #country == country_name, # USA
      project == project_code, # English-language Wikipedia
      page_title %in% str_replace_all(mpox_keywords, " ", "_") # keywords
    ) |>
    mutate(
      date = as_date(date_str), # add date col
      page_title = str_replace_all(page_title, "_", " ") # remove underscores
    ) |>
    rename(country_long = country)
  
  return(df)
}

if (!file.exists(here("3-data/wikipedia/pageviews-dp-en.csv"))) { ###
  # load, clean, and combine dataframes
  pageviews_dp <- map_dfr(file_paths, import_pageviews_tsv) # ~1 min
  
  # save daily differentially-private pageviews data
  write_csv(pageviews_dp, here("3-data/wikipedia/pageviews-dp-en.csv")) ###
} else pageviews_dp <- read_csv(here("3-data/wikipedia/pageviews-dp-en.csv")) ###


# Query total monthly country-level pageviews data -----------------------------
# # function to get and parse data for a given month
# fetch_pageviews_for_month <- function(yyyy_mm) {
#   year <- str_extract(yyyy_mm, "^\\d{4}")
#   month <- str_extract(yyyy_mm, "\\d{2}$")
# 
#   url <- glue("https://wikimedia.org/api/rest_v1/metrics/pageviews/top-by-country/{project_code}/all-access/{year}/{month}") 
# 
#   response <- GET(url) # API request
# 
#   if (status_code(response) == 200) {
#     data <- content(response, "parsed", type = "application/json") # parse JSON
#     countries_list <- data$items[[1]]$countries
#     countries_df <- map_dfr(countries_list, ~ as_tibble(.x)) |>
#       select(iso2 = country, pageviews_ceil = views_ceil) |>
#       mutate(year = as.numeric(year), month = as.numeric(month)) # add year and month
#   } else {
#     return(NULL) # if fails, return NULL
#   }
#   return(countries_df)
# }
# 
# # combine data for all months
# month_sequence <- date_sequence |>
#   str_remove("-\\d\\d$") |>
#   unique()
# pageviews_total <- map_df(month_sequence, fetch_pageviews_for_month)


# Prepare data -----------------------------------------------------------------
## Pageview data ---------------------------------------------------------------
# aggregate pageviews for "Mpox" and "Monkeypox virus" pages
pageviews_agg <- pageviews |>
  group_by(project, date) |>
  summarize(total_pageviews = sum(views)) |>
  ungroup() |>
  mutate(page_title = "Mpox/Monkeypox virus") |>
  relocate(c(page_title, date), .before = total_pageviews) |> 
  arrange(date)


## Differential-private pageview data ------------------------------------------
# aggregate differential-private pageviews for "Mpox" and "Monkeypox virus" pages
pageviews_dp_agg <- pageviews_dp |>
  group_by(project, country_long, iso2, date) |>
  summarize(pageviews_dp = sum(pageviews_dp)) |>
  ungroup() |>
  mutate(page_title = "Mpox/Monkeypox virus") |>
  relocate(c(page_title, date), .before = pageviews_dp)

# calculate daily differential-private pageviews by country 
pageviews_dp_totals <- pageviews_dp_agg |> 
  group_by(project, page_title, date) |> 
  summarize(total_pageviews_dp = sum(pageviews_dp))

# calculate daily share of differential-private pageviews by country
pageviews_dp_pct <- pageviews_dp_agg |> 
  group_by(project, country_long, iso2, page_title, date) |> 
  summarize(pageviews_dp = sum(pageviews_dp), .groups = "drop") |> 
  left_join(pageviews_dp_totals, by = join_by(project, page_title, date)) |> 
  mutate(pageviews_dp_pct = pageviews_dp / total_pageviews_dp) |> 
  select(project, country_long, iso2, page_title, date, pageviews_dp_pct)


## Merge pageview data with private-differential pageview data -----------------
pageviews_mpox <- left_join(pageviews_dp_pct, pageviews_agg, by = join_by(project, page_title, date)) |> 
  mutate(pageviews_est = total_pageviews * pageviews_dp_pct) |> 
  select(-total_pageviews, -pageviews_dp_pct) |> 
  filter(country_long %in% c("Australia", "Canada", "Germany", "India", "Ireland", "United Kingdom", "United States of America")) ### countries with engough data

## Calculate weekly pageviews --------------------------------------------------
pageviews_wk <- pageviews_mpox |>
  mutate(
    date = ceiling_date(date, unit = "weeks", week_start = 3),
    year = year(date),
    month = month(date)
  ) |>
  group_by(project, page_title, country_long, iso2, year, month, date) |>
  summarize(pageviews_est = sum(pageviews_est)) |>
  ungroup() |>
  relocate(c(year, month, date), .before = pageviews_est)


# Explore data -----------------------------------------------------------------
# plot daily pageviews
pageviews_mpox |>
  ggplot(aes(x = date, y = pageviews_est, color = country_long)) +
  geom_line() +
  geom_hline(yintercept = 450, color = "red", linetype = "dashed") + 
  scale_x_date(
    limits = c(min(pageviews_mpox$date), max(pageviews_mpox$date)),
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
  facet_wrap(~country_long, ncol = 1, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none")

# save plot
ggsave(here("5-visualization/wiki-pageviews-en-daily.png"))


# plot weekly pageviews
pageviews_wk |>
  ggplot(aes(x = date, y = pageviews_est, color = country_long)) +
  geom_line(alpha = 0.75) +
  geom_hline(yintercept = 450, color = "red", linetype = "dashed") + # min pageviews
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
  facet_wrap(~country_long, ncol = 1, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none")

# save plot
ggsave(here("5-visualization/wiki-pageviews-en-weekly.png"))
