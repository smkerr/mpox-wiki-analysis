# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================


# Prepare data =================================================================
# load ISO code reference table
load(here("3-data/ref/iso_codes.RData"))

# merge ISO codes 
mpox_df <- left_join(cases_df, iso_ref, by = join_by(iso3)) |> 
  relocate(iso2, .before = iso3) |> 
  full_join(pageviews_mpox, by = join_by(iso2, date)) |>
  select(
    country, iso2, iso3, date, pageviews_est, cases, cases_moving_avg
  ) |> 
  group_by(iso2) |>
  mutate(country = if_else(
    is.na(country),
    first(country[!is.na(country)]),
    country
  )) |> 
  mutate(iso3 = if_else(
    is.na(iso3),
    first(iso3[!is.na(iso3)]),
    iso3
  )) |> 
  ungroup() |> 
  complete(fill = list(
   project = "en.wikipedia", ###
   cases = 0,
   cases_moving_avg = 0
  )) |>
  filter(
    !is.na(pageviews_est), ### remove countries without enough English Wikipedia pageviews
    date >= as_date("2022-05-01"), ###
    date <= as_date("2022-10-09") ###
  )

### test 
mpox_df <- mpox_df |> 
  filter(iso3 == "USA")


# Explore data =================================================================
# plot mpox-related pageviews and mpox cases
coeff <- 0.0021 # value to transform scales
p <- mpox_df |>
  ggplot(aes(x = date)) +
  geom_col(aes(y = cases_moving_avg / coeff, fill = "7-day average confirmed cases")) +
  geom_line(aes(y = pageviews_est, color = "Mpox-related pageviews"), linewidth = 1) +
  facet_wrap(~country) +
  scale_x_date(date_labels = "%b %Y") +
  scale_y_continuous(
    name = "Mpox-related pageviews", # first axis
    sec.axis = sec_axis(~ . * coeff, name = "7-day average confirmed cases"), # second axis
    labels = scales::comma
  ) +
  scale_fill_brewer(type = "qual", palette = 3) +
  labs(
    #title = country_name,
    x = NULL,
    fill = NULL,
    color = NULL,
    caption = "Source: World Health Organization via Our World in Data"
  ) +
  theme_minimal() +
  theme(legend.position = "top")
p

# save plot
ggsave(here("5-visualization/mpox-cases-&-wiki-pageviews.png"), height = 7.75, width = 10)


# Estimate effect of PHEIC declaration on mpox attention =======================
library(nlme) # linear & nonlinear mixed-effects models
DATE_PHEIC_DECLARATION <- as_date("2022-07-23")

# visualize data
mpox_df |> 
  ggplot(aes(x = date, y = pageviews_est)) +
  geom_line() +
  geom_vline(xintercept = DATE_PHEIC_DECLARATION, linetype = "dashed", color = "red") +
  labs(title = "Interrupted Time-Series Analysis of PHEIC Declaration", x = "Time", y = "Outcome") + 
  theme_minimal()

# prepare data 
its_df <- mpox_df |> 
  select(country, iso2, iso3, project, date, pageviews_est) |> 
  mutate(intervention = ifelse(date <= DATE_PHEIC_DECLARATION, "Pre", "Post"))
  
# Simple linear model approach
lm_model <- lm(data = its_df, pageviews_est ~ date + intervention)
summary(lm_model)

# For more complex analysis, considering autocorrelation
its_model <- lme(data = its_df, pageviews_est ~ date * intervention, random = ~1|date)
summary(its_model)

# visualize model results
its_df |> 
  add_column(fitted = fitted(its_model)) |> 
  ggplot(aes(x = date)) +
  geom_line(aes(y = pageviews_est), color = "grey") +
  geom_line(aes(y = fitted), color = "blue") +
  geom_vline(xintercept = DATE_PHEIC_DECLARATION, linetype = "dashed", color = "red") +
  labs(title = "ITS Analysis: Observed vs. Fitted", x = "Time", y = "Outcome")

# Implement methodology used by Du et al. 
du_df <- mpox_df |> 
  select(country, iso2, iso3, project, date, Y = pageviews_est, cases, cases_moving_avg) |> 
  arrange(date) |> 
  mutate(
    X1 = seq_along(unique(mpox_df$date)), # time variable 
    # TODO: Could I just as easily use the date variable?
    X2 = ifelse(date <= DATE_PHEIC_DECLARATION, 0, 1),
    X3 = c(rep(0, which(date == DATE_PHEIC_DECLARATION)), 1:(length(date) - which(date == DATE_PHEIC_DECLARATION)))
  ) 

# Fitting the ITS model
its_model <- lm(data = du_df, Y ~ X1 + X2 + X3)

# Summarizing the model to inspect the coefficients
summary(its_model) 

# Extract coefficients
beta1 <- coef(its_model)["X1"] # β1 (the slope before the PHEIC declaration)
beta2 <- coef(its_model)["X2"] # β2 (the immediate change in level after the PHEIC declaration)
beta3 <- coef(its_model)["X3"]

# Calculate new slope after PHEIC declaration
new_slope_after_PHEIC <- beta1 + beta3 # β1 + β3 (the new slope after the PHEIC declaration)

# Visualize map of daily average change of pageviews after PHEIC declaration
library(sf)
library(spData)
PHEIC_df <- world |> 
  mutate(
    beta1 = ifelse(iso_a2 == "US", beta1, NA),
    beta3 = ifelse(iso_a2 == "US", beta3, NA),
    new_slope_after_PHEIC = ifelse(iso_a2 == "US", new_slope_after_PHEIC, NA)
    )

library(tmap)
qtm(PHEIC_df, fill = "beta1") # slope before PHEIC declaration
qtm(PHEIC_df, fill = "beta3") # immediate change after PHEIC declaration
qtm(PHEIC_df, fill = "new_slope_after_PHEIC") # slope after PHEIC declaration 

  
# Test time-lag correlations between online search activity & new cases ========
# Function to calculate Spearman correlation with lag
calculate_correlation_with_lag <- function(data, outcome_var, lagged_var, lag) {
  # Shift the lagged variable manually
  if (lag > 0) {
    shifted_cases <- c(rep(NA, lag), data[[lagged_var]][1:(nrow(data) - lag)])
  } else if (lag < 0) {
    shifted_cases <- c(data[[lagged_var]][(-lag + 1):nrow(data)], rep(NA, -lag))
  } else { # lag == 0
    shifted_cases <- data[[lagged_var]]
  }
  
  # Remove rows where either variable is NA to ensure proper comparison
  clean_data <- data %>%
    mutate(shifted_cases = shifted_cases) %>%
    filter(!is.na(shifted_cases), !is.na(data[[outcome_var]]))
  
  # Perform the correlation test with the shifted data
  cor.test(clean_data$shifted_cases, clean_data[[outcome_var]], method = "spearman") %>% 
    tidy() %>% 
    rename(rho = estimate, S = statistic)
}

# Assuming 'du_df' is your data frame, 'Y' is your outcome variable, and 'cases_moving_avg' is your lagged variable
lags <- c(-21, -14, -7, 0, 7, 14, 21)
results <- purrr::map(lags, ~ calculate_correlation_with_lag(data = du_df, outcome_var = "Y", lagged_var = "cases_moving_avg", .x))

# Combine results into a single dataframe
final_results <- bind_rows(results, .id = "lag") %>% 
  mutate(lag = as.integer(lags[as.integer(lag)]))
final_results

# correlations will contain the Spearman correlation coefficients for each lag
## negative lags correspond with the time-lag effect of new cases on pageviews
## positive lags correspond with the time-lag effect of pageviews on new cases 




# Test stationarity ============================================================
#> Implement ADF test to check for stationarity in both cases and pageviews.
#> Non-stationary data can lead to spurious results in subsequent analyses.

# check stationarity of pageviews time-series
adf.test(mpox_df$pageviews_est, alternative = "stationary")
# p-value > significance level of 0.05, so non-stationary
# still need to difference...

# check stationarity of mpox cases time-series
adf.test(mpox_df$cases_moving_avg, alternative = "stationary")
# p-value > significance level of 0.05, so non-stationary
# still need to difference...

# first-order differencing
mpox_df <- mpox_df |>
  mutate(
    pageviews_diff1 = c(NA, diff(pageviews_est, lag = 1, differences = 1)),
    cases_moving_avg_diff1 = c(NA, diff(cases_moving_avg, lag = 1, differences = 1))
  )

# plot first-order differenced pageviews
mpox_df |>
  ggplot(aes(x = date, y = pageviews_diff1)) +
  geom_line() +
  labs(
    title = "Daily pageviews for mpox-related pages",
    #subtitle = country_name,
    x = NULL,
    y = "Views",
    caption = "Source: World Health Organization via Our World in Data"
  ) +
  theme_minimal()

# plot first-order differenced cases
mpox_df |>
  ggplot(aes(x = date, y = cases_moving_avg_diff1)) +
  geom_line() +
  labs(
    title = "Daily mpox cases",
    #subtitle = country_name,
    x = NULL,
    y = "Cases",
    caption = "Source: Wikimedia Foundation"
  ) +
  theme_minimal()

# check stationarity of first-order differenced pageviews time-series
mpox_df |>
  filter(!is.na(pageviews_diff1)) |>
  pull(pageviews_diff1) |>
  adf.test(alternative = "stationary")
# p-value < significance level of 0.05, so stationary

# check stationarity of first-order differenced mpox cases time-series
mpox_df |>
  filter(!is.na(cases_moving_avg_diff1)) |>
  pull(cases_moving_avg_diff1) |>
  adf.test(alternative = "stationary")
# p-value < significance level of 0.05, so stationary


# Develop VAR model ============================================================
#> Develop a VAR model to understand the dynamic relationship between the two time series. This model will help in capturing the temporal interdependencies and feedback mechanisms between Wikipedia traffic and mpox cases.

# convert to a time-series object
mpox_ts <- mpox_df |>
  filter(!is.na(pageviews_diff1), !is.na(cases_moving_avg_diff1)) |>
  select(pageviews_diff1, cases_moving_avg_diff1) |>
  ts()

# determine optimal lag length for VAR model based on information criteria (e.g., AIC)
lag_selection <- VARselect(mpox_ts, lag.max = 24, type = "both")
optimal_lag <- lag_selection$selection["AIC(n)"]

# fit VAR model
var_model <- VAR(mpox_ts, p = optimal_lag, type = "both")
var_model

# summary of VAR model
summary(var_model)


# Apply Granger causality test =================================================
#> Apply the Granger causality test within the VAR framework to assess whether Wikipedia traffic volumes can be considered a predictor of mpox case trajectories.

# test if pageviews Granger-cause mpox cases
granger_pageviews <- causality(var_model, cause = "pageviews_diff1")
granger_pageviews

# test if mpox cases Granger-cause mpox cases
granger_cases <- causality(var_model, cause = "cases_moving_avg_diff1")
granger_cases

#> The key aspect of the result to look at is the p-value. If the p-value is less than your significance level (commonly set at 0.05), you can conclude that there is evidence to suggest that the cause variable Granger-causes the other variable(s) in the VAR model

