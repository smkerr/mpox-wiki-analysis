# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================

# TODO: Could also implement the simpler version from Du et al. to possibly better 
# capture overall trends
# TODO: Impement the global version

# TODO: Should I also model this for initial news of outbreak?

# Estimate effect of PHEIC declaration on mpox attention =======================
# WHO declared mpox PHEIC on 23 July 2022
DATE_PHEIC_DECLARATION <- as_date("2022-07-23")

# top 20 countries by total pageviews 
top_20_pageviews <- pageviews_df |> 
  reframe(.by = country, total_pageviews = sum(est_pageviews)) |> 
  arrange(-total_pageviews) |> 
  pull(country) |> 
  head(20)

# visualize PHEIC impact on top 20 countries (just to get an idea)
pageviews_df |> 
  filter(country %in% top_20_pageviews) |> 
  mutate(country = factor(country, levels = top_20_pageviews)) |> 
  filter(iso3 == "USA") |> 
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
  filter(page_title %in% c("Mpox", "Monkeypox virus")) |> # only for mpox-specific articles
  reframe( # aggregate pageviews
    .by = c(country, iso2, iso3, date), 
    est_pageviews = sum(est_pageviews, na.rm = TRUE),
    pct_est_pageviews = est_pageviews / pageviews_ceil,
    pageviews_ceil
    ) |> 
  group_by(country) |> 
  filter(sum(pct_est_pageviews > 0) >= 30) |> 
  mutate(
    X1 = row_number(), # time variable
    # TODO: Could I just as easily use the date variable?
    X2 = ifelse(date < DATE_PHEIC_DECLARATION, 0, 1),
    X3 = c(rep(0, which(date == DATE_PHEIC_DECLARATION)), 1:(length(date) - which(date == DATE_PHEIC_DECLARATION)))
  ) |> 
  filter(n_distinct(X2) == 2) |> # only countries with data before and after intervention
  ungroup() |> 
  rename(Y = pct_est_pageviews) |> 
  nest(.by = c(iso3)) # nest data by country 

# function to fit ITS model for a given country, adjusting for autocorrelation
fit_its_model <- function(data) {
  its_model <- lm(data = data, Y ~ X1 + X2 + X3)
  return(its_model)
}

# function to visualize ITS model results
viz_its_model <- function(data, model) {
  p <- data |> 
    add_column(fitted = fitted(model)) |> 
    ggplot(aes(x = date)) +
    geom_line(aes(y = Y), color = "grey") +
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
  beta1 <- its_model$coefficients["X1"] # slope before PHEIC declaration
  beta2 <- its_model$coefficients["X2"] # immediate change in level
  beta3 <- its_model$coefficients["X3"] # new slope after
  
  return(c(iso3 = iso3_code, beta1, beta2, beta3))
})

# Convert the list to a dataframe
coefficients_df <- bind_rows(coefficients_list) |> 
  mutate(across(-iso3, as.numeric))

# Merge with world map data
data(World)
PHEIC_df <- left_join(World, coefficients_df, by = c("iso_a3" = "iso3")) |> 
  mutate(new_slope_after_PHEIC = X1 + X3)

# Visualize 
tm_shape(PHEIC_df) + # slope before PHEIC declaration
  tm_polygons( 
    fill = "X1", 
    fill.scale = tm_scale_intervals(values = "purple_green"),
    fill.legend = tm_legend(title = "Slope before PHEIC declaration")
)
tm_shape(PHEIC_df) + # immediate change after PHEIC declaration
  tm_polygons( 
    fill = "X2", 
    fill.scale = tm_scale_intervals(values = "purple_green"),
    fill.legend = tm_legend(title = "Immediate change after PHEIC declaration")
    )
tm_shape(PHEIC_df) + # slope after PHEIC declaration
  tm_polygons( 
    fill = "new_slope_after_PHEIC", 
    fill.scale = tm_scale_intervals(values = "purple_green"),
    fill.legend = tm_legend(title = "Slope after PHEIC declaration")
  )


# Global level =================================================================
# create global level data
its_global <- its_nested |> 
  unnest(cols = data) |> 
  reframe(
    .by = date,
    est_pageviews = sum(est_pageviews, na.rm = TRUE),
    pageviews_ceil = sum(pageviews_ceil),
    pct_est_pageviews = est_pageviews / pageviews_ceil
  ) |> 
  mutate(
    X1 = row_number(), # time variable
    # TODO: Could I just as easily use the date variable?
    X2 = ifelse(date < DATE_PHEIC_DECLARATION, 0, 1),
    X3 = c(rep(0, which(date == DATE_PHEIC_DECLARATION)), 1:(length(date) - which(date == DATE_PHEIC_DECLARATION)))
    ) |> 
  rename(Y = pct_est_pageviews)

# visualize PHEIC impact on global attention
its_global |> 
  ggplot(aes(x = date, y = Y)) +
  geom_line() +
  geom_vline(xintercept = DATE_PHEIC_DECLARATION, linetype = "dashed", color = "red") +
  scale_x_date(date_labels = "%b\n%Y") +
  scale_y_continuous(labels = label_comma()) +
  labs(title = "Interrupted Time-Series Analysis of PHEIC Declaration", x = "Time", y = "Outcome") +
  theme_minimal()

its_model_global <- fit_its_model(its_global)

# Extract coefficients
global_beta1 <- its_model_global$coefficients["X1"] # slope before PHEIC declaration
global_beta2 <- its_model_global$coefficients["X2"] # immediate change in level
global_beta3 <- its_model_global$coefficients["X3"] # new slope after
new_slope_after_PHEIC <- global_beta1 + global_beta3
