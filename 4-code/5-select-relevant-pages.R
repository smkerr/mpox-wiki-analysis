# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================


# Setup ========================================================================
# load mpox cases & pageviews 
mpox_df <- read_csv(here("3-data/output/mpox-data.csv"))


# Calculate correlation between cases and pageviews ============================
# # TODO: Revisit this.... This next part should supersede this section ...
# article_correlations <- mpox_df |>
#   group_by(country, iso2, iso3, wikidata_id, page_id, page_title) |> 
#   filter(sum(pct_pageviews > 0) > 1) |> # remove articles estimated to have zero pageviews
#   ungroup() |> 
#   group_by(country, iso2, iso3) |> 
#   nest() |> 
#   mutate(correlations = map(data, ~ .x |> 
#                               group_by(page_title) |> 
#                               summarize(
#                                 n = n(),
#                                 correlation = cor(pct_pageviews, 
#                                                   cases, 
#                                                   use = "complete.obs"), 
#                                 .groups = 'drop') |> 
#                               arrange(-correlation)
#                             )) |> 
#   select(-data) |> 
#   unnest(correlations) |> 
#   ungroup()
#   
# # Save results
# write_csv(article_correlations, here("3-data/output/article-correlations.csv"))


# Prepare data =================================================================
# create list of dates
date_sequence <- unique(mpox_df$date)

# cases by date
cases_by_date <- mpox_df |> distinct(date, cases)

# total monthly pageviews by date
total_pageviews_by_date <- mpox_df |> distinct(date, pageviews_ceil)

# complete missing data
mpox_df <- mpox_df |> 
  distinct(page_title, date) |> 
  complete(page_title, date = date_sequence) |> 
  left_join(mpox_df, by = join_by(page_title, date)) |> 
  group_by(page_title) |> 
  fill(country, iso2, iso3, project, wikidata_id, page_id, .direction = "updown") |> 
  ungroup() |> 
  select(-cases) |> 
  left_join(cases_by_date, by = join_by(date)) |> 
  select(-pageviews_ceil) |> 
  left_join(total_pageviews_by_date, join_by(date)) |> 
  select(country, iso2, iso3, project, wikidata_id, page_id, page_title, date, cases, pct_pageviews, pageviews, pageviews_ceil) |>
  group_by(page_title) |> 
  filter(sum(pageviews > 450, na.rm = TRUE) > 5) |> # at least 5 weeks of observations for a given article
  ungroup()
  #complete(fill = list(pageviews = 450)) |> # assume missing values
  #mutate(pct_pageviews = pageviews / pageviews_ceil)

### 
mpox_df |> 
  filter(pageviews > 450) |>
  count(page_title, sort = TRUE)
  

# Try out leaving NAs missing....

#complete(fill = list(pageviews = 0, pct_pageviews = 0, cases = 0))  # fill in missing zero
# mpox_df |> 
#   group_by(country, iso2, iso3) |> 
#   mutate(pageviews = case_when(
#     sum(pageviews, na.rm = TRUE) == 0 ~ NA,
#     !is.na(pageviews) ~ pageviews,
#     TRUE ~ 450 # supression threshold = 450
#   ))
  



# Calculate time-lagged correlation between cases and pageviews ================
# Function to calculate Spearman correlation given lag
calculate_correlation_with_lag <- function(data, outcome_var, lagged_var, lag) {
  
  # Shift the lagged variable manually
  if (lag > 0) {
    shifted_cases <- c(rep(NA, lag), data[[lagged_var]][1:(nrow(data) - lag)])
  } else if (lag < 0) {
    shifted_cases <- c(data[[lagged_var]][(-lag + 1):nrow(data)], rep(NA, - lag))
  } else { # lag == 0
    shifted_cases <- data[[lagged_var]]
  }
  
  # Remove rows where either variable is NA to ensure proper comparison
  clean_data <- data |> 
    mutate(shifted_cases = shifted_cases) |> 
    filter(!is.na(shifted_cases), !is.na(data[[outcome_var]]))
  
  results <- try({
    cor.test(clean_data$shifted_cases, clean_data[[outcome_var]], method = "spearman") |>
      tidy() |>
      rename(rho = estimate, S = statistic) 
  }, silent = TRUE)
  
  if (inherits(results, "try-error")) {
    return(data.frame(rho = NA, S = NA, p.value = NA, lag = lag))
  } else {
    return(results)
  }
  
}


# define lag range (in weeks)
lags <- -28:28 
# TODO: should be informed by attention decay analysis

# initialize empty dataframe
final_results <- data.frame()

for (title in unique(mpox_df$page_title)) {
  filtered_df <- mpox_df |> filter(page_title == title)
  results <- map(lags,
                 ~ calculate_correlation_with_lag(
                   data = filtered_df, 
                   outcome_var = "pct_pageviews", 
                   lagged_var = "cases", 
                   lag = .x)
  ) |> 
    bind_rows(.id = "lag") |> # combine lag correlations into single dataframe
    mutate(
      lag = as.integer(lags[as.integer(lag)]),
      page_title = title
    ) 
  
  final_results <- bind_rows(final_results, results) |> 
    relocate(c(page_title, lag), .before = everything())
}

final_results

# TODO: Properly interpret results ... 

# Visualize results ============================================================
# Plot Spearman correlation coefficient by article
final_results |> 
  ggplot(aes(x = lag, y = rho, color = page_title)) + 
  geom_line() +
  facet_wrap(~page_title, ncol = 3) + 
  theme_minimal()
  
# Plot p-value by article
final_results |> 
  ggplot(aes(x = lag, y = p.value, color = page_title)) + 
  geom_line() +
  facet_wrap(~page_title) + 
  theme_minimal()

# Plot Spearman correlation coeffcient and p-value by article
final_results |> 
  ggplot() +
  geom_line(aes(x = lag, y = rho, color = "Spearman correllation coef")) + 
  geom_line(aes(x = lag, y = p.value, color = "p-value")) + 
  facet_wrap(~page_title) + 
  theme_minimal()

# p-value
ggplot(final_results, aes(x = lag, y = p.value, color = page_title)) + 
  geom_line() + 
  facet_wrap(~page_title, scale = "fixed", ncol = 3) + 
  theme_minimal() +
  theme(legend.position = "none")

# Spearman coefficient
ggplot(final_results, aes(x = lag, y = rho, color = page_title)) + 
  geom_line() + 
  facet_wrap(~page_title, ncol = 3) + 
  theme_minimal() +
  theme(legend.position = "none")

# Order articles by average Spearman correlation coefficient
order_by_coefficients <- final_results |> 
  reframe(
    .by = page_title,
    avg_rho = mean(rho)
  ) |> 
  arrange(avg_rho) |> 
  pull(page_title)

# heatmap of Spearman correlation coefficients
final_results |> 
  mutate(page_title = factor(page_title, levels = order_by_coefficients)) |> 
  ggplot(aes(x = lag, y = page_title, fill = rho)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    limits = c(-1, 1),
    midpoint = 0,
    low = "#91bfdb", 
    mid = "#ffffbf",
    high = "#fc8d59"
    ) +
  scale_x_continuous(n.breaks = 10) +
  labs(
    title = "Time lag correlation of Wikipedia page views and weekly mpox cases",
    x = "Time lag (weeks)",
    y = NULL,
    fill = "Spearman \ncorrelation \ncoefficient"
  ) +
  theme_minimal()

# Order articles by average p-value
order_by_p.values <- final_results |> 
  reframe(
    .by = page_title,
    avg_p.value = mean(p.value)
  ) |> 
  arrange(-avg_p.value) |> 
  pull(page_title)

# heatmap of p-values
final_results |> 
  mutate(page_title = factor(page_title, levels = order_by_p.values)) |> 
  ggplot(aes(x = lag, y = page_title, fill = p.value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    limits = c(0, 1),
    midpoint = 0.5,
    low = "#fc8d59",
    mid = "#ffffbf",
    high = "#91bfdb"
  ) +
  scale_x_continuous(n.breaks = 10) +
  labs(
    title = "Significance of time lag correlation of Wikipedia page views and weekly mpox cases",
    x = "Time lag (weeks)",
    y = NULL,
    fill = "p-value"
  ) +
  theme_minimal()

# TODO: Implement select criteria ..............................................
