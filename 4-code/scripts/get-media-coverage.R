library(httr)
library(jsonlite)

api_key <- "f2fec2e793ac49f386fc686c2257d865"
search_term <- "Mpox"

response <- GET(url = paste0("https://newsapi.org/v2/everything?q=", search_term, "&apiKey=", api_key))

content <- content(response, "text")
parsed <- fromJSON(content)

total_results <- parsed$totalResults
print(total_results)

parsed$articles |> names()
parsed$articles |> str()

# Flatten the 'source' column
news_df <- parsed$articles |> 
  mutate(source_id = source$id,  # Create new column for source ID
         source_name = source$name) |>   # Create new column for source name
  select(-source)  # Remove the original nested 'source' column
head(news_df)

news_df <- news_df |> mutate(date = as_date(ymd_hms(publishedAt))) |> 
  filter(description != "[Removed]")
ggplot(news_df, aes(x = date)) +
  geom_bar() +
  scale_x_date(limits = c(min(news_df$date), max(news_df$date))) + 
  theme_minimal()


###

library(httr)
library(jsonlite)
library(lubridate)

# Define your search parameters
api_key <- "f2fec2e793ac49f386fc686c2257d865"
search_term <- "Mpox"
start_date <- as.Date("2022-07-01")
end_date <- as.Date("2022-07-31")
current_date <- start_date

all_articles <- list()
index <- 1

while (current_date <= end_date) {
  fromDate <- current_date
  toDate <- current_date + weeks(1)  # Increment by 1 week
  
  # Make sure the toDate does not exceed the end_date
  if (toDate > end_date) {
    toDate <- end_date
  }
  
  response <- GET(url = paste0("https://newsapi.org/v2/everything?q=", search_term, 
                               "&from=", format(fromDate, "%Y-%m-%d"),
                               "&to=", format(toDate, "%Y-%m-%d"),
                               "&apiKey=", api_key))
  
  content <- content(response, "text")
  parsed <- fromJSON(content)
  
  if (!is.null(parsed$articles)) {
    all_articles[[index]] <- parsed$articles
    index <- index + 1
  }
  
  # Move to the next period
  current_date <- toDate + weeks(1)
}


# GNews API --------------------------------------------------------------------
api_key <- "d7cd982fa29aa243410b5e6f8800bb0c"
search_query <- "mpox"  # Example search term

response <- GET(
  url = glue("https://gnews.io/api/v4/search?q={URLencode(search_query)}&max=10&apikey={api_key}"))

parsed_content <- content(response, as = "parsed")

articles <- as.data.frame(parsed_content$articles)

parsed_content$totalArticles
parsed_content$articles |> str()

# Assuming your list is named 'articles_list'
# Convert the list to a dataframe
articles_df <- do.call(rbind, lapply(parsed_content$articles, function(x) {
  data.frame(
    title = x$title,
    description = x$description,
    content = x$content,
    url = x$url,
    image = x$image,
    publishedAt = x$publishedAt,
    source_name = x$source$name,
    source_url = x$source$url,
    stringsAsFactors = FALSE  # Ensure that text columns are not converted to factors
  )
}))

# If you have tibble and dplyr, you can optionally convert it to a tibble for nicer printing
articles_df <- as_tibble(articles_df)

# View the resulting dataframe
print(articles_df)
