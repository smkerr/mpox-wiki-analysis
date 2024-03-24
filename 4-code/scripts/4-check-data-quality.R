# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================


# Data quality checks ----------------------------------------------------------
# mpox data 
agg_probable <- agg_total |> 
  filter(total_probable_cases > 0) |> 
  group_by(country) |> 
  mutate(
    total_cases = sum(total_confirmed_cases, total_probable_cases),
    pct_probable_cases = total_probable_cases / total_cases * 100) |> 
  arrange(desc(pct_probable_cases)) 

agg_probable |> 
  ggplot(aes(x = reorder(country, pct_probable_cases), y = pct_probable_cases)) + 
  geom_col() + 
  labs(
    title = "Percentage of probable cases, by country",
    subtitle = "data as of 31 Dec 2023 17:00 CET",
    x = "",
    y = "Percentage of probable cases"
  ) + 
  coord_flip() + 
  theme_minimal()

# Wikipedia data
# frequency of projects and page names
pageviews[1:2] |> 
  map(~ tabyl(.x)) |> 
  set_names(names(pageviews[1:2]))

# % missing by column
pageviews |> 
  map_df(~sum(is.na(.)/length(.)) * 100)

# calculate max redirects
max_redirects <- pageviews |> 
  tabyl(redirect_name, page_name, show_na = FALSE) |> 
  select(where(is.numeric)) |> 
  max()

# redirects heat map
pageviews_tbl <- pageviews |> 
  tabyl(redirect_name, page_name, show_na = FALSE) |> 
  gt() |> 
  cols_label(
    redirect_name = "Redirected search"
  ) |> 
  tab_header(
    title = "Redirects for user views",
    subtitle = "English Wikipedia"
  ) |> 
  tab_spanner(
    label = "Articles",
    columns  = !redirect_name
  ) |>
  data_color(
    columns = !redirect_name,
    colors = col_numeric(
      palette = c("white", "red"),
      domain = c(0, max_redirects))
  ) 
pageviews_tbl


# Check differentially private pageviews ---------------------------------------
df |> 
  tabyl(country, project) |> 
  group_by(country)
slice_max()

labels <- df |> 
  tabyl(project, country) |>
  select(project)
totals <- df |> 
  tabyl(project, country) |> 
  select(-project) |> 
  rowSums()
combo <- cbind(labels, totals) |> 
  arrange(totals)
combo |> 
  mutate(project = factor(combo$project, levels = combo$project)) |> 
  filter(totals >= 15) |> 
  ggplot(aes(x = project, y = totals)) + 
  geom_col() + 
  labs(
    title = "Number of overall differentially private pageviews",
    subtitle = "for select countries on 15 June 2022",
    y = "Number of pageviews (differentially private)",
    x = NULL
  ) +
  theme_minimal() + 
  coord_flip()

pie(totals, labels = labels, man = "Share of differentially private pageviews by language")

combo |> 
  slice_max(totals, n = 10) |> 
  mutate(project = factor(project, levels = slice_max(combo, totals, n = 10)$project)) |> 
  ggplot(aes(x = "", y = totals, fill = project)) + 
  geom_col(width = 1, color = "white") + 
  coord_polar(theta = "y") + 
  labs(
    title = "Share of pageviews (differentially private) by language",
    subtitle = "Top 10 most common languages in selected countries",
    fill = "Wikipedia project"
  ) +
  theme_void() 
