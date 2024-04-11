library(rentrez)

# PubMed -----------------------------------------------------------------------
search_results <- entrez_search(db = "pubmed", term = "Monkeypox", use_history = TRUE)
article_summaries <- entrez_summary(db = "pubmed", id = search_results$ids)
publication_dates <- lapply(article_summaries, function(x) x$pubdate)

batch_size <- 100  # Define batch size
total_hits <- search_results$count  # Total number of articles found
num_batches <- ceiling(total_hits / batch_size)  # Calculate the number of batches

# Initialize a list to store the data from each batch
articles_data <- list()

for (i in seq_len(num_batches)) {
  # Calculate the range of records for the current batch
  start_record <- ((i - 1) * batch_size) + 1
  end_record <- min(i * batch_size, total_hits)
  
  # Fetch article information for the current batch
  articles_info <- entrez_summary(db = "pubmed", 
                                  web_history = search_results$web_history,
                                  retstart = start_record, 
                                  retmax = batch_size)
  
  # Extract relevant information from each article in the batch
  batch_data <- lapply(articles_info, function(article) {
    list(
      uid = article$uid,
      pubdate = article$pubdate,
      epubdate = article$epubdate,
      source = article$source,
      authors = article$authors,
      title = article$title,
      lang = article$lang,
      pubtype = article$pubtype
    )
  })
  
  # Store the processed batch data
  articles_data <- c(articles_data, batch_data)
  
  # Respect NCBI's usage limits by pausing between requests
  Sys.sleep(1)  # Pause for 1 second; adjust based on the total number of requests
}
length(articles_data)

# Initialize an empty dataframe to store the combined data
articles_df <- data.frame()

for(article_id in names(articles_data)) {
  article <- articles_data[[article_id]]
  
  # Create a dataframe from the current article
  article_df <- data.frame(
    uid = article$uid,
    pubdate = article$pubdate,
    epubdate = article$epubdate,
    source = article$source,
    authors = paste(article$authors, collapse = ", "),
    title = article$title,
    lang = article$lang,
    pubtype = paste(article$pubtype, collapse = ", ")
  )
  
  # Append the current article's dataframe to the combined dataframe
  articles_df <- rbind(articles_df, article_df)
}

# PubMed Central ---------------------------------------------------------------
search_results <- entrez_search(db = "pmc", term = "Monkeypox", use_history = TRUE)
article_summaries <- entrez_summary(db = "pmc", id = search_results$ids)
publication_dates <- lapply(article_summaries, function(x) x$pubdate)

batch_size <- 100  # Define batch size
total_hits <- search_results$count  # Total number of articles found
num_batches <- ceiling(total_hits / batch_size)  # Calculate the number of batches

# Initialize a list to store the data from each batch
articles_data <- list()

for (i in seq_len(num_batches)) {
  # Calculate the range of records for the current batch
  start_record <- ((i - 1) * batch_size) + 1
  end_record <- min(i * batch_size, total_hits)
  
  # Fetch article information for the current batch
  articles_info <- entrez_summary(db = "pmc", 
                                  web_history = search_results$web_history,
                                  retstart = start_record, 
                                  retmax = batch_size)
  
  # Extract relevant information from each article in the batch
  batch_data <- lapply(articles_info, function(article) {
    list(
      uid = article$uid,
      pubdate = article$pubdate,
      epubdate = article$epubdate,
      source = article$source,
      authors = article$authors,
      title = article$title,
      lang = article$lang,
      pubtype = article$pubtype
    )
  })
  
  # Store the processed batch data
  articles_data <- c(articles_data, batch_data)
  
  # Respect NCBI's usage limits by pausing between requests
  Sys.sleep(1)  # Pause for 1 second; adjust based on the total number of requests
}
length(articles_data)

# Initialize an empty dataframe to store the combined data
articles_df <- data.frame()

for(article_id in names(articles_data)) {
  article <- articles_data[[article_id]]
  
  # Create a dataframe from the current article
  article_df <- data.frame(
    uid = article$uid,
    pubdate = article$pubdate,
    epubdate = article$epubdate,
    source = article$source,
    authors = paste(article$authors, collapse = ", "),
    title = article$title,
    lang = article$lang,
    pubtype = paste(article$pubtype, collapse = ", ")
  )
  
  # Append the current article's dataframe to the combined dataframe
  articles_df <- rbind(articles_df, article_df)
}


# Europe PubMed Central --------------------------------------------------------
library(europepmc)

epmc_df <- europepmc::epmc_search(query = '"Mpox" OR "Monkeypox"')


tt_oa <- europepmc::epmc_hits_trend("Mpox", period = 1995:2023, synonym = TRUE)
tt_oa
# we use ggplot2 for plotting the graph
ggplot(tt_oa, aes(year, query_hits / all_hits)) + 
  geom_point() + 
  geom_line() +
  xlab("Year published") + 
  ylab("Proportion of articles on Malaria in Europe PMC")


# Via the API
library(httr)
library(jsonlite)

#query <- '"Mpox" OR "Monkeypox"'
query <- "Monkeypox"
response <- GET(
  url = paste0("https://www.ebi.ac.uk/europepmc/webservices/rest/search?query=", 
               query, "&resulttype=core&format=json",
               pageSize, "&page=", page)
  content <- content(response, "text")
  parsed <- fromJSON(content)
  epmc_df <- parsed$resultList$result
  
  
  # Initial setup
  query <- "Monkeypox"
  pageSize <- 100
  page <- 1
  totalResults <- NA
  all_articles <- list()
  
  # Get initial data to check the total number of results
  response <- GET(url = paste0("https://www.ebi.ac.uk/europepmc/webservices/rest/search?query=", query, "&resulttype=core&format=json&pageSize=", pageSize, "&page=", page))
  content <- content(response, "text")
  parsed <- fromJSON(content)
  totalResults <- parsed$hitCount
  
  # Calculate total pages
  totalPages <- ceiling(totalResults / pageSize)
  
  # Loop through all pages and collect articles
  for (page in 1:totalPages) {
    response <- GET(url = paste0("https://www.ebi.ac.uk/europepmc/webservices/rest/search?query=", query, "&resulttype=core&format=json&pageSize=", pageSize, "&page=", page))
    content <- content(response, "text")
    parsed <- fromJSON(content)
    all_articles[[page]] <- parsed$resultList$result
  }
  
  # Combine all articles into a single list or data frame as needed
  # This step depends on the structure of the articles and how you want to process them
  all_articles |> names()
