# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================


# Identify pages linked in "Mpox" and "Monkeypox virus" pages ------------------
# browseURL("https://en.wikipedia.org/wiki/Mpox")
mpox_links <- page_links(
  language = "en",
  project = "wikipedia",
  page = "Mpox",
  clean_response = TRUE,
  limit = 500
) |>
  pluck(1) |>
  pluck(4) |>
  map_chr(~ .x["title"])

# browseURL("https://en.wikipedia.org/wiki/Monkeypox_virus")
mpox_virus_links <- page_links(
  language = "en",
  project = "wikipedia",
  page = "Monkeypox_virus",
  clean_response = TRUE,
  limit = 500
) |>
  pluck(1) |>
  pluck(4) |>
  map_chr(~ .x["title"])

# de-duplicate pages
mpox_related_links <- c(mpox_links, mpox_virus_links) |> unique()

# save results
write_xlsx(tibble(page_title = mpox_related_links), here("data/output/mpox_linked_pages.xlsx"))

# manually classify pages as mpox-related ...
mpox_keywords <- read_excel(here("data/output/mpox_linked_pages_classified.xlsx")) |>
  filter(mpox_related == 1) |> # limit to mpox-related pages
  pull(page_title)

# Manually identify more mpox-related pages ------------------------------------
# source: WHO Mpox Case Reporting Form (CRF)
mpox_keywords_manual <- c(
  "Back pain",               
  "Balanitis",               
  "Chills",                  
  "Cough",                   
  "Diarrhea",                
  "Edema",                   
  "Fever",                   
  "Headache",                
  "Lesion",                  
  "List of skin conditions", 
  "Myalgia",                 
  "Myocarditis",             
  "Skin condition",          
  "Sore throat",             
  "Urethritis",              
  "Vomiting" 
  )


# Save page names --------------------------------------------------------------
mpox_keywords_extended <- c(mpox_keywords, mpox_keywords_manual) # combine keywords
mpox_keywords_extended <- str_replace_all(mpox_keywords_extended, " ", "_") # reformat

# save keywords
save(mpox_keywords, file = here("data/output/mpox_keywords.RData"))
