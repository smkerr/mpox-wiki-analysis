library(rentrez)

# Set API Key 
NCBI_API_KEY <- Sys.getenv("NCBI_API_KEY")

# Check available databases 
entrez_dbs()

# Get brief description of database
entrez_db_summary(db = "pubmed") 

# Check available search terms
entrez_db_searchable(db = "pubmed")

search_results <- entrez_search(
  db = "pubmed", 
  term = "Monkeypox AND 2022:2023[PDAT]", 
  use_history = TRUE,
  api_key = NCBI_API_KEY 
  )

library(xml2)

studies_df <- data.frame()

for (seq_start in seq(1, search_results$count, 50)) {
  studies_fetched <- entrez_fetch(db = "pubmed", web_history = search_results$web_history,
                       rettype = "xml", retmax = 50, retstart = seq_start)
  studies_xml <- studies_fetched |> read_xml()
  studies <- xml_find_all(studies_xml, ".//PubmedArticle")
  
  # Loop through each article and extract details
  studies_details <- lapply(studies, function(study) {
    data.frame(
      id = xml_text(xml_find_first(study, ".//PMID")),
      title = xml_text(xml_find_first(study, ".//ArticleTitle")),
      pubdate_year = xml_text(xml_find_first(study, ".//PubMedPubDate/Year")),
      pubdate_month = xml_text(xml_find_first(study, ".//PubMedPubDate/Month")),
      pubdate_day = xml_text(xml_find_first(study, ".//PubMedPubDate/Day"))
    )
  })
  
  # Combine individual article details into a single dataframe
  studies_df <- bind_rows(studies_df, studies_details)
  glue("{seq_start + 49} sequences downloaded")
  
  # Polite querying 
  Sys.sleep(0.1)
}

studies_df


esummaries <- entrez_summary(
  db = "pubmed",
  web_history = search_results$web_history,
)

studies_list <- extract_from_esummary(esummaries, elements = c("uid", "title", "sortpubdate"), simplify = FALSE)
studies_df <- map_df(studies_list, ~{
  tibble(
    id = .x$id,
    title = .x$title,
    date = .x$sortpubdate 
    )
  }) |> 
  mutate(
    date = ymd_hm(date),
    date = as_date(date)
    )

# plot daily number of published studies
studies_df |> 
  ggplot(aes(x = date)) +
  geom_bar()

# plot daily number of published studies
studies_df |> 
  ggplot(aes(x = date)) +
  geom_bar() +
  theme_minimal()

# plot weekly number of published studies
studies_df |> 
  mutate(date = floor_date(date, unit = "months")) |> 
  ggplot(aes(x = date)) +
  geom_bar() + 
  theme_minimal()

# investigate outliers
studies_df |> 
  filter(date > today()) # several have supposedly been published in the future

studies_df |> 
  filter(date < as_date("2024-01-01")) # several were published last year but made it into the dataset somehow...



###########
search_results <- entrez_search(db = "pubmed", term = "Monkeypox")
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
