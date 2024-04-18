# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================


# Identify pages linked in "Mpox", "Monkeypox", "Monkeypox virus" pages ========
# write function to extract titles of linked pages 
get_linked_pages <- function(page_title) {
  linked_pages <- page_links(
    language = "en",
    project = "wikipedia",
    page = str_replace_all(page_title, " ", "_"),
    clean_response = TRUE,
    limit = 500
  ) |>
    pluck(1) |>
    pluck(4) |>
    map_chr(~ .x["title"])
  
  return(linked_pages)
}

# define mpox pages
mpox_pages <- c("Mpox", "Monkeypox", "Monkeypox virus")

# get linked pages
mpox_linked_pages <- map(mpox_pages, get_linked_pages) |> unlist() |> unique()

# save results
write_xlsx(tibble(page_title = mpox_linked_pages), here("3-data/output/mpox-linked-pages.xlsx"))

# manually classify pages as mpox-related ...
mpox_relevant_pages <- read_excel(here("3-data/output/mpox-linked-pages-classified.xlsx")) |>
  filter(relevant == 1) |> # limit to mpox-related pages
  arrange(page_title) |> 
  pull(page_title)

# Manually identify additional mpox-related pages ==============================
# source: WHO Mpox Case Reporting Form (CRF)
mpox_relevant_pages_manual <- c(
  "Back pain",               
  "Chills",                  
  "Fever",                   
  "Headache",                
  "Lesion",                  
  "List of skin conditions", 
  "Myalgia",                 
  "Skin condition",          
  "Sore throat",             
  "Urethritis"            
  )


# Combine relevant page names ==================================================
mpox_pages_extended <- c(mpox_pages, mpox_relevant_pages, mpox_relevant_pages_manual) |> unique()

# save keywords
save(mpox_pages_extended, file = here("3-data/output/mpox-pages-extended.RData"))

# TODO: To be saved as an .txt file or excel file to make it easer to review? 
