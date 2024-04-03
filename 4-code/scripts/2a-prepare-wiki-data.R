# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================


# Setup ========================================================================
# define time period
start_date <- ymd("2022-01-01") ### try out with first year of outbreak
end_date <- ymd("2022-12-31")
date_sequence <- seq.Date(from = start_date, to = end_date, by = "day")

# load ISO reference table
load(here("3-data/ref/iso_codes.RData"))

# define mpox-specific pages
#mpox_pages  <- c("Mpox", "Monkeypox", "Monkeypox virus")

# load relevant mpox pages
load(here("3-data/output/mpox_pages_extended.RData"))


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
alt_page_title_df <- map_df(str_replace_all(mpox_pages_extended, " ", "_"), get_alt_page_titles) |>
  rename(page_title = `*`) |> 
  bind_rows(tibble( # append mpox-related pages for English-language Wikipedia
    lang = rep("en", length(mpox_pages_extended)),
    page_title = mpox_pages_extended,
    page_title_en = mpox_pages_extended
    ))


# Get daily pageviews by project ===============================================
# write function to query mpox-related pageviews for a given project
get_daily_pageviews <- function(data, project_code) {
  
  data <- data |> filter(lang == project_code) 
  
  pageviews_df <- data.frame() # initialize df to store project-level pageview data
  
  for (page in data$page_title) {
    
    url <- glue("https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/{paste0(project_code, '.wikipedia')}/all-access/user/{str_replace_all(page, ' ', '_')}/daily/{str_remove_all(start_date, '-')}/{str_remove_all(end_date, '-')}") # endpoint
    
    response <- GET(url) # API request
    
    if (status_code(response) == 200) {
      df <- content(response, "parsed", type = "application/json") # parse JSON
      pageviews <- map_dfr(df$items, ~as_tibble(.x)) |> 
        mutate(date = as_date(ymd_h(timestamp))) |> # timestamp to date type
        select(project, page_title = article, date, pageviews = views) |> 
        arrange(project, date, page_title)
      pageviews_df <- bind_rows(pageviews_df, pageviews)  
    } else {
      next # if fails, move on to next
    }
  }
  write_csv(pageviews_df, here(glue("3-data/wikipedia/pageviews/pageviews-{project_code}.csv")))
}

# combine pageview data for all language projects 
if (!file.exists(here("3-data/wikipedia/pageviews.csv"))) {
  map_df(unique(alt_page_title_df$lang), ~ get_daily_pageviews(data = alt_page_title_df, project_code = .x))
  file_paths <- list.files(here("3-data/wikipedia/pageviews"), full.names = TRUE)
  pageviews <- map_dfr(file_paths, read_csv)
  write_csv(pageviews, here("3-data/wikipedia/pageviews.csv"))
} else {
  pageviews <- read_csv(here("3-data/wikipedia/pageviews.csv"))
}

# Translate page names to English
pageviews <- alt_page_title_df |> 
  mutate(project = paste0(lang, ".wikipedia")) |> 
  distinct(project, page_title, page_title_en) |> 
  right_join(pageviews, by = join_by(project, page_title)) |> 
  select(-page_title) |> 
  rename(page_title = page_title_en)

# TODO: Need to take into account that many of these pages were created after the 
# outbreak started. Possibly exclude articles less than X days old when implementing
# inclusion criteria...


# Get monthly project views by country =========================================
# write function to query and parse data for a given project and month
get_monthly_project_views <- function(project_code, yyyy_mm_sequence) {
  
  project_views_df <- data.frame() # initialize df to store project view data
  
  for (yyyy_mm in yyyy_mm_sequence) {
    
    year <- str_extract(yyyy_mm, "^\\d{4}")
    month <- str_extract(yyyy_mm, "\\d{2}$")
    
    url <- glue("https://wikimedia.org/api/rest_v1/metrics/pageviews/top-by-country/{paste0(project_code, '.wikipedia')}/all-access/{year}/{month}") # endpoint
  
    response <- GET(url) # API request
    
    if (status_code(response) == 200) {
      data <- content(response, "parsed", type = "application/json") # parse JSON
      projet_views_list <- data$items[[1]]$countries
      project_views <- map_dfr(projet_views_list, ~ as_tibble(.x)) |>
        mutate(year = as.numeric(year), month = as.numeric(month), project = paste0(project_code, ".wikipedia")) |> 
        select(project, iso2 = country, year, month, pageviews_ceil = views_ceil) |> 
        arrange(project, year, month, -pageviews_ceil)
      project_views_df <- bind_rows(pageviews_df, pageviews)  
    } else {
      next # if fails, move on to next
    }
  }
  write_csv(pageviews_df, here(glue("3-data/wikipedia/project-views/project-views-{project_code}.csv")))
}

# combine & save monthly project views
if (!file.exists(here("3-data/wikipedia/project-views-by-country.csv"))) {
  month_sequence <- date_sequence |> str_remove("-\\d\\d$") |> unique()
  combination_df <- crossing(project_code = alt_page_title_df$lang, month = month_sequence)
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

# TODO: Why is there still missing value in the country col?


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
