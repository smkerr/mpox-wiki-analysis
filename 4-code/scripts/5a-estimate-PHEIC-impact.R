# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================

# TODO: It's hard to believe that the WHO PHEIC announcement caused public attention
# to decrease in all countries but the US and the Philippines... Need to make sure 
# the code is working as expected...

# Estimate effect of PHEIC declaration on mpox attention =======================
# WHO declared mpox PHEIC on 23 July 2022
DATE_PHEIC_DECLARATION <- as_date("2022-07-23")

# visualize data
mpox_df |> 
  ggplot(aes(x = date, y = pageviews_est)) +
  geom_line() +
  geom_vline(xintercept = DATE_PHEIC_DECLARATION, linetype = "dashed", color = "red") +
  facet_wrap(~country, scale = "free_y") +
  labs(title = "Interrupted Time-Series Analysis of PHEIC Declaration", x = "Time", y = "Outcome") + 
  theme_minimal()

# prepare data 
its_nested <- mpox_df |> 
  select(country, iso2, iso3, date, pageviews_est) |> 
  mutate(intervention = ifelse(date <= DATE_PHEIC_DECLARATION, 0, 1)) |> 
  group_by(country) |> 
  filter(n_distinct(intervention) == 2) |> # only countries with data before and after intervention
  ungroup() |>
  nest(.by = c(iso2)) # nest data by country 

# write function to fit ITS model for a given country, adjusting for autocorrelation
fit_its_model <- function(data) {
  its_model <- lme(pageviews_est ~ date * intervention, random = ~1|date, data = data)
  return(its_model)
}

# write function to visualize ITS model results
viz_its_model <- function(data, model) {
  p <- data |> 
    add_column(fitted = fitted(model)) |> 
    ggplot(aes(x = date)) +
    geom_line(aes(y = pageviews_est), color = "grey") +
    geom_line(aes(y = fitted), color = "blue") +
    geom_vline(xintercept = DATE_PHEIC_DECLARATION, linetype = "dashed", color = "red") +
    facet_wrap(~country) +
    scale_y_continuous(labels = scales::comma_format()) +
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

# Unique list of country ISO3 codes
country_codes <- unique(its_df$iso2)

# Extract coefficients by country
coefficients_list <- map(country_codes, function(iso2_code) {
  # Filter data for the country
  its_model <- its_df |> filter(iso2 == iso2_code) |> pull(model) |> pluck(1)
  
  # Extract coefficients
  beta1 <- its_model$coefficients$fixed["date"] # slope before PHEIC declaration
  beta2 <- its_model$coefficients$fixed["intervention"] # immediate change in level
  beta3 <- its_model$coefficients$fixed["date:intervention"] # new slope after
  
  return(c(iso2 = iso2_code, beta1, beta2, beta3))
})

# Convert the list to a dataframe
coefficients_df <- bind_rows(coefficients_list) |> 
  mutate(across(-iso2, as.numeric))

# Merge with world map data
PHEIC_df <- left_join(world, coefficients_df, by = c("iso_a2" = "iso2"))

# Visualize 
qtm(PHEIC_df, fill = "date") # slope before PHEIC declaration
qtm(PHEIC_df, fill = "intervention", midpoint = NA) # immediate change after PHEIC declaration
qtm(PHEIC_df, fill = "date:intervention") # slope after PHEIC declaration 
