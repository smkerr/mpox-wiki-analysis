# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Author: Steve Kerr
# Date: April 2024
# ==============================================================================


# Set API Key 
NCBI_API_KEY <- Sys.getenv("NCBI_API_KEY")

if (file.exists(here("3-data/mpox-studies/mpox-total-studies.csv"))) {
  studies_df <- read_csv(here("3-data/mpox-studies/mpox-total-studies.csv"))  
} else {

  # Search database
  search_results <- entrez_search(
    db = "pubmed", 
    term = "Monkeypox AND 2022:2024[PDAT]", 
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
    select(id, title, date) |> 
    filter(
      date >= as_date("2022-01-01"),
      date <= as_date("2024-02-27")
    ) 
  
  # 18 studies with no titles
  studies_df |> filter(title == "[Not Available].") |> count()
  
  # 41 duplicate titles 
  count(studies_df, title, sort = TRUE) |> filter(n > 1) |> summarise(n = sum(n) - n())
  
  # Remove studies with missing or duplicate titles 
  studies_df <- studies_df |> 
    filter(title != "[Not Available].") |> 
    arrange(date) |> 
    group_by(title) |>
    slice(1) |> # keep earliest observation
    ungroup() 
  
  # Save results
  write_csv(studies_df, here("3-data/mpox-studies/mpox-total-studies.csv"))
}
