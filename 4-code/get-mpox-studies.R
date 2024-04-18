library(rentrez)

# Set API Key 
NCBI_API_KEY <- Sys.getenv("NCBI_API_KEY")

# Search database
search_results <- entrez_search(
  db = "pubmed", 
  term = "Monkeypox AND 2022:2023[PDAT]", 
  use_history = TRUE,
  api_key = NCBI_API_KEY 
  )

# Initialize empty dataframe
studies_df <- data.frame()

# Take a batched approach to querying for study details
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
  print(glue("{seq_start + 49} sequences downloaded"))
  
  # Polite querying 
  Sys.sleep(0.1)
}

# Prepare data
studies_df <- studies_df |> 
  mutate(date = ymd(glue("{pubdate_year}-{pubdate_month}-{pubdate_day}"))) |> 
  select(id, title, date)

# plot daily number of published studies
studies_df |> 
  ggplot(aes(x = date)) +
  geom_bar() +
  theme_minimal()

# plot monthly number of published studies
studies_df |> 
  mutate(date = floor_date(date, unit = "weeks", week_start = 1)) |> 
  ggplot(aes(x = date)) +
  geom_bar() + 
  theme_minimal()

# plot monthly number of published studies
studies_df |> 
  mutate(date = floor_date(date, unit = "months")) |> 
  ggplot(aes(x = date)) +
  geom_bar() + 
  theme_minimal()

# investigate outliers
studies_df |> 
  filter(date > today()) # several have supposedly been published in the future

# Clean data
studies_df <- studies_df |> 
  filter(
    date >= as_date("2022-01-01"),
    date <= as_date("2023-12-31")
  ) 
# TODO: De-duplicate studies

# Save results
write_csv(studies_df, here("3-data/mpox-studies/mpox-total-studies.csv"))
