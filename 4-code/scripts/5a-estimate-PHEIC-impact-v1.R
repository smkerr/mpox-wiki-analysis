# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================

# TODO: Could also implement the simpler version from Du et al. to possibly better 
# capture overall trends

# Estimate effect of PHEIC declaration on mpox attention =======================

# WHO declared mpox PHEIC on 23 July 2022
DATE_PHEIC_DECLARATION <- as_date("2022-07-23")

# top 20 countries by total pageviews 
top_20_pageviews <- pageviews_df |> 
  reframe(.by = country, total_pageviews = sum(est_pageviews)) |> 
  arrange(-total_pageviews) |> 
  pull(country) |> 
  head(20)

# visualize data
pageviews_df |> 
  filter(country %in% top_20_pageviews) |> 
  mutate(country = factor(country, levels = top_20_pageviews)) |> 
  ggplot(aes(x = date, y = pct_est_pageviews)) +
  geom_line() +
  geom_vline(xintercept = DATE_PHEIC_DECLARATION, linetype = "dashed", color = "red") +
  facet_wrap(~country, scale = "free_y") +
  scale_x_date(date_labels = "%b\n%Y") +
  scale_y_continuous(labels = label_comma()) +
  labs(title = "Interrupted Time-Series Analysis of PHEIC Declaration", x = "Time", y = "Outcome") + 
  theme_minimal()

ggsave(filename = here("5-visualization/wiki-pageviews-ITS-top-20.png"), height = 7.75, width = 10)

# prepare data 
its_nested <- pageviews_df |> 
  group_by(country) |> 
  filter(sum(pct_est_pageviews > 0) >= 30) |> # at least 30 days of pageviews data
  ungroup() |> 
  select(country, iso2, iso3, date, pct_est_pageviews) |> 
  mutate(intervention = ifelse(date < DATE_PHEIC_DECLARATION, 0, 1)) |> 
  group_by(country) |> 
  filter(n_distinct(intervention) == 2) |> # only countries with data before and after intervention
  ungroup() |>
  nest(.by = c(iso3)) # nest data by country 

# write function to fit ITS model for a given country, adjusting for autocorrelation
fit_its_model <- function(data) {
  its_model <- lme(pct_est_pageviews ~ date * intervention, random = ~1|date, data = data)
  return(its_model)
}

# write function to visualize ITS model results
viz_its_model <- function(data, model) {
  p <- data |> 
    add_column(fitted = fitted(model)) |> 
    ggplot(aes(x = date)) +
    geom_line(aes(y = pct_est_pageviews), color = "grey") +
    geom_line(aes(y = fitted), color = "blue") +
    geom_vline(xintercept = DATE_PHEIC_DECLARATION, linetype = "dashed", color = "red") +
    facet_wrap(~country) +
    scale_x_date(date_labels = "%b\n%Y") +
    scale_y_continuous(labels = label_percent()) +
    labs(title = "ITS Analysis: Observed vs. Fitted", x = NULL, y = "Pageviews") + 
    theme_minimal()
  return(p)
}

# fit ITS model to each country, adjusting for autocorrelation
its_df <- its_nested |> mutate(model = map(data, fit_its_model)) 

# visualize ITS model results
its_plots <- map2(its_df$data, its_df$model, viz_its_model)
for (plot in its_plots) {
  print(plot)
}

# Extract coefficients by country
coefficients_list <- map(unique(its_df$iso3), function(iso3_code) {
  # Filter data for the country
  its_model <- its_df |> filter(iso3 == iso3_code) |> pull(model) |> pluck(1)
  
  # Extract coefficients
  beta1 <- its_model$coefficients$fixed["date"] # slope before PHEIC declaration
  beta2 <- its_model$coefficients$fixed["intervention"] # immediate change in level
  beta3 <- its_model$coefficients$fixed["date:intervention"] # new slope after
  
  return(c(iso3 = iso3_code, beta1, beta2, beta3))
})

# Convert the list to a dataframe
coefficients_df <- bind_rows(coefficients_list) |> 
  mutate(across(-iso3, as.numeric))

# Merge with world map data
data(World)
PHEIC_df <- left_join(World, coefficients_df, by = c("iso_a3" = "iso3"))

# Visualize 
qtm(PHEIC_df, fill = "date") # slope before PHEIC declaration
qtm(PHEIC_df, fill = "intervention", midpoint = NA) # immediate change after PHEIC declaration
qtm(PHEIC_df, fill = "date:intervention") # slope after PHEIC declaration 
