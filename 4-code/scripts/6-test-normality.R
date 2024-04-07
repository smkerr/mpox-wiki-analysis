# ==============================================================================
# Assessing Public Attention Towards 2022-2023 Mpox Outbreak Using Wikipedia
# Steve Kerr
# ==============================================================================


# Determine whether pageview data is normally distributed  =====================
## Histograms -------------------------------------------------------------------
# pageviews
mpox_df |> 
  filter(est_pct_pageviews > 0) |> 
  ggplot(aes(x = est_pct_pageviews)) + 
  geom_histogram(bins = 30, color = "black") +
  facet_wrap(~country, scale = "free") +
  scale_x_continuous(labels = label_percent()) +
  theme_minimal()

# pageviews (logged)
mpox_df |> 
  filter(est_pct_pageviews > 0) |> 
  ggplot(aes(x = log(est_pct_pageviews))) +  # logged
  geom_histogram(bins = 30, color = "black") +
  facet_wrap(~country, scale = "free") +
  theme_minimal()

## Q-Q plots -------------------------------------------------------------------
# pageviews
mpox_df |>  
  filter(est_pct_pageviews > 0) |> 
  ggplot(aes(sample = est_pct_pageviews)) + 
  geom_qq() + 
  geom_qq_line() +
  facet_wrap(~country, scale = "free") +
  scale_x_continuous(labels = label_percent()) +
  theme_minimal()

# pageviews (logged)
mpox_df |> 
  filter(est_pct_pageviews > 0) |> 
  ggplot(aes(sample = log(est_pct_pageviews))) + 
  geom_qq() + 
  geom_qq_line() +
  facet_wrap(~country, scale = "free") +
  theme_minimal()

## Normality tests -------------------------------------------------------------
# write function to apply normality tests
test_normality <- function(data, var) {
  # Anderson-Darling Test 
  ad_result <- ad.test(data[[var]])
  ad_logged_result <- ad.test(log(data[[var]] + 1)) # add 1 to avoid log(0)
  # Lilliefors (Kolmogorov-Smirnov Test) 
  lilliefors_result <- lillie.test(data[[var]])
  lilliefors_logged_result <- lillie.test(log(data[[var]] + 1)) # add 1 to avoid log(0)
  # Shapiro-Wilk Test
  shapiro_result <- shapiro.test(data[[var]])
  shapiro_logged_result <- shapiro.test(log(data[[var]] + 1)) # add 1 to avoid log(0)
  
  # Create a data frame from the results
  tibble(
    ad_p_value = ad_result$p.value,
    ad_logged_p_value = ad_logged_result$p.value,
    lilliefors_p_value = lilliefors_result$p.value,
    lilliefors_logged_p_value = lilliefors_logged_result$p.value,
    shapiro_p_value = shapiro_result$p.value,
    shapiro_logged_p_value = shapiro_logged_result$p.value
  )
}

# Apply normality tests to each country
pageviews_results <- mpox_df |> 
  filter(est_pct_pageviews > 0) |> 
  group_by(country) |> 
  group_modify(~test_normality(data = .x, var = "est_pct_pageviews")) |> 
  ungroup()

# No p-values are >0.01, therefore we can conclude that our data does is not 
# normally distributed
pageviews_results |> 
  filter(if_any(where(is.numeric), ~ . > 0.01))


# Determine whether mpox case data is normally distributed =====================
## Histograms -------------------------------------------------------------------
# mpox cases
mpox_df |> 
  group_by(country) |> 
  filter(sum(cases) > 30, cases_moving_avg > 0) |> 
  ungroup() |> 
  ggplot(aes(x = cases_moving_avg)) + 
  geom_histogram(bins = 30, color = "black") +
  facet_wrap(~country, scale = "free") +
  theme_minimal()

# mpox cases (logged)
mpox_df |> 
  group_by(country) |> 
  filter(sum(cases) > 30, cases_moving_avg > 0) |> 
  ungroup() |> 
  ggplot(aes(x = log(cases))) +  # logged
  geom_histogram(bins = 30, color = "black") +
  facet_wrap(~country, scale = "free") +
  theme_minimal()

## Q-Q plots -------------------------------------------------------------------
# mpox cases
mpox_df |>  
  group_by(country) |> 
  filter(sum(cases) > 30, cases_moving_avg > 0) |> 
  ungroup() |> 
  ggplot(aes(sample = cases_moving_avg)) + 
  geom_qq() + 
  geom_qq_line() +
  facet_wrap(~country, scale = "free") +
  theme_minimal()

# mpox cases (logged)
mpox_df |> 
  group_by(country) |> 
  filter(sum(cases) > 30, cases_moving_avg > 0) |> 
  ungroup() |> 
  ggplot(aes(sample = log(cases_moving_avg))) + 
  geom_qq() + 
  geom_qq_line() +
  facet_wrap(~country, scale = "free") +
  theme_minimal()

## Normality tests -------------------------------------------------------------
# Apply normality tests to each country
cases_results <- mpox_df |> 
  group_by(country) |> 
  filter(sum(cases) > 30, cases_moving_avg > 0) |> 
  group_modify(~test_normality(data = .x, var = "cases_moving_avg")) |> 
  ungroup()

# No p-values are >0.01, therefore we can conclude that our data does is not 
# normally distributed
cases_results |> 
  filter(if_any(where(is.numeric), ~ . > 0.01))

# Conclusion -------------------------------------------------------------------
#> According to all normality tests performed, the data fails to meet the
#> assumption that they are normally distributed, therefore the Pearson correlation 
#> coefficient is not appropriate for this context. Instead, the Spearman correlation
#> can be used.
