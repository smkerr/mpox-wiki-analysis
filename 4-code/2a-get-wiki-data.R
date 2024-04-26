# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Author: Steve Kerr
# Date: April 2024
# ==============================================================================


# Setup ===============================================================
# Load packages
pacman::p_load(
  dplyr, 
  glue,
  here,
  httr,
  lubridate,
  purrr,
  readr,
  stringr,
  tidyr
)

# Mpox-related articles
load(here("3-data/output/article-selection/mpox-pages-extended.RData"))


# Query page titles in other languages =========================================
get_alt_page_titles <- function(keyword) {
  url <- glue("https://en.wikipedia.org/w/api.php?action=query&titles={keyword}&prop=langlinks&lllimit=max&format=json")

  response <- GET(url) # API request

  if (status_code(response) == 200) {
    data <- content(response, "parsed", type = "application/json") # parse JSON
    lang_list <- data$query$pages[[1]]$langlinks
    lang_df <- map_dfr(lang_list, ~ as_tibble(.x))
  } else {
    return(NULL) # if fails, return NULL
  }
  return(lang_df)
}

# combine all alternate page titles
mpox_pages_extended <- map_df(str_replace_all(mpox_pages_extended, " ", "_"), get_alt_page_titles) |>
  rename(alt_page_title = `*`) |>
  pull(alt_page_title)


# Download Daily Pageviews (Differential Privacy) ==============================
if (!dir.exists((here("3-data/wikipedia/pageviews-differential-privacy")))) {
  # Download pageview data from 1 Jan 2022 - 5 Feb 2023 
  # source: https://analytics.wikimedia.org/published/datasets/country_project_page_historical/00_README.html
  start_date <- ymd("2022-01-01") 
  end_date <- ymd("2023-02-05") 
  date_sequence <- seq.Date(from = start_date, to = end_date, by = "day")
  
  for (date in date_sequence) {
    # file path for download
    dest_path <- here(glue("3-data/wikipedia/pageviews-differential-privacy/{as_date(date)}.tsv"))

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
  
  
  # Download pageview data from 6 Feb 2023 - 27 Feb 2024
  # sourceL https://analytics.wikimedia.org/published/datasets/country_project_page/00_README.html
  start_date <- ymd("2023-02-06") 
  end_date <- ymd("2024-02-27") 
  date_sequence <- seq.Date(from = start_date, to = end_date, by = "day")
  
  for (date in date_sequence) {
    # file path for download
    dest_path <- here(glue("3-data/wikipedia/pageviews-differential-privacy/{as_date(date)}.tsv"))
    
    # skip if file already exists
    if (file.exists(dest_path)) next
    
    # URL
    url <- glue("https://analytics.wikimedia.org/published/datasets/country_project_page/{as_date(date)}.tsv")
    
    # try to download file
    result <- try(download.file(url, destfile = dest_path), silent = TRUE)
    
    # check for error
    if (inherits(result, "try-error")) {
      message("Failed to download: ", url)
      next
    }
  }
}


# Load Daily Pageviews (Differential Privacy) ==================================
# define file paths to data
file_paths <- list.files(
  path = "3-data/wikipedia/pageviews-differential-privacy",
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
      "wikidata_id",
      "pageviews"
    )
  )

  # extract relevant pages
  df <- df |>
    filter(page_title %in% str_replace_all(mpox_pages_extended, " ", "_")) |> 
    filter(
      !(page_title == "Utah" & project != "su.wikipedia.org"), # "utah" means "vomiting" in Sundanese
      !(page_title == "Hosta" & project != "hu.wikipedia.org") # "hosta" means "cough" in Hungarian
      ) |> 
    mutate(
      date = as_date(date_str), 
      page_title = str_replace_all(page_title, "_", " "), # remove underscores
      country = as.character(country),
      country = ifelse(iso2 == "US", "United States", country), # align US country name
    ) |>
    rename(country_long = country)
  
  if (nrow(df) == 0) {
    df <- data.frame(
      country_long = NA_character_,
      iso2 = NA_character_,
      project = NA_character_,
      page_id = NA_real_,
      page_title = NA_character_,
      wikidata_id = NA_character_,
      pageviews = NA_real_
    )
  }
  return(df)
}


if (!file.exists(here("3-data/wikipedia/pageviews-differential-private.csv"))) { 
  # load, clean, and combine dataframes
  pageviews_df <- map_dfr(file_paths, import_pageviews_tsv) |> 
    drop_na()
  write_csv(pageviews_df, here("3-data/wikipedia/pageviews-differential-private.csv")) 
} else {
  pageviews_df <- read_csv(here("3-data/wikipedia/pageviews-differential-private.csv"))
}


# Get Monthly Total Pageviews ==================================================
# function to get and parse data for a given month
get_pageviews_for_month <- function(project_code, yyyy_mm) {
  year <- str_extract(yyyy_mm, "^\\d{4}")
  month <- str_extract(yyyy_mm, "\\d{2}$")

  url <- glue("https://wikimedia.org/api/rest_v1/metrics/pageviews/top-by-country/{project_code}/all-access/{year}/{month}")

  response <- GET(url) # API request

  if (status_code(response) == 200) {
    data <- content(response, "parsed", type = "application/json") # parse JSON
    countries_list <- data$items[[1]]$countries
    countries_df <- map_dfr(countries_list, ~ as_tibble(.x)) |>
      select(iso2 = country, pageviews_ceil = views_ceil) |>
      mutate(
        project = project_code,
        year = as.numeric(year), 
        month = as.numeric(month)
        ) 
  } else {
    return(NULL) # if fails, return NULL
  }
  return(countries_df)
}


if (file.exists(here("3-data/wikipedia/project-views-monthly.csv"))) {
  pageviews_total <- read_csv(here("3-data/wikipedia/project-views-monthly.csv"))
} else {
  # create list of project codes 
  project_codes <- unique(pageviews$project)
  
  # combine data for all months
  start_date <- ymd("2022-01-01") 
  end_date <- ymd("2024-02-27") 
  month_sequence <- seq.Date(from = start_date, to = end_date, by = "day") |> 
    str_remove("-\\d\\d$") |>
    unique()
  
  # generate all possible combinations of project codes and months
  combo_df <- expand.grid(project = project_codes, date = month_sequence) |> 
    mutate(project = as.character(project), date = as.character(date))
  
  # get total project views by country
  pageviews_total <- map2(combo_df$project, combo_df$date, get_pageviews_for_month) |> 
    list_rbind() 
  
  write_csv(pageviews_total, here("3-data/wikipedia/project-views-monthly.csv"))
}


# Prepare data =================================================================
# Daily pageviews
pageviews_daily <- pageviews_df |>  
  # Expand to include missing dates
  complete(page_title, date = seq.Date(min(pageviews_df$date), to = max(pageviews_df$date), by = 1)) |>
  group_by(page_title) |> 
  # Fill in missing info
  fill(country_long, iso2, project, wikidata_id, page_id, .direction = "updown") |> 
  ungroup() |> 
  # Normalize by dividing by total monthly country project views 
  mutate(year = year(date), month = month(date)) |> 
  left_join(pageviews_total, by = join_by(iso2, project, year, month), relationship = "many-to-one") |> 
  mutate(pct_pageviews = pageviews / pageviews_ceil) |> 
  select(country_long, iso2, project, wikidata_id, page_id, page_title, date, pct_pageviews, pageviews, pageviews_ceil)

# Weekly pageviews
pageviews_weekly <- pageviews_df |>  
  # Expand to include missing dates
  complete(page_title, date = seq.Date(min(pageviews_df$date), to = max(pageviews_df$date), by = 1)) |>
  group_by(page_title) |> 
  # Fill in missing info
  fill(country_long, iso2, project, wikidata_id, page_id, .direction = "updown") |> 
  ungroup() |>
  # Calculate weekly pageviews
  mutate(date = floor_date(date, unit = "weeks"), year = year(date), month = month(date)) |>
  reframe(
    .by = c(country_long, iso2, project, wikidata_id, page_id, page_title, year, month, date),
    pageviews = sum(pageviews, na.rm = TRUE)
  ) |>
  # Normalize by dividing by total monthly country project views 
  left_join(pageviews_total, by = join_by(iso2, project, year, month), relationship = "many-to-one") |> 
  mutate(pct_pageviews = pageviews / pageviews_ceil) |> 
  select(country_long, iso2, project, wikidata_id, page_title, page_id, date, pct_pageviews, pageviews, pageviews_ceil)


# Save data ====================================================================
write_csv(pageviews_daily, here("3-data/wikipedia/pageviews-daily.csv"))
write_csv(pageviews_weekly, here("3-data/wikipedia/pageviews-weekly.csv"))
