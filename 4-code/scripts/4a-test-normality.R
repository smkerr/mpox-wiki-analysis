# Script to determine whether data is normally distributed =====================

# Setup ------------------------------------------------------------------------

# TODO: modify code that plots and tests are applied to each country's distribution

# Pageviews --------------------------------------------------------------------
# histogram
mpox_df |> 
  ggplot(aes(x = pageviews_est)) + 
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  theme_minimal()

# histogram (logged)
mpox_df |> 
  ggplot(aes(x = log(pageviews_est))) +  # logged
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  theme_minimal()

# Q-Q plot 
mpox_df |>  
  ggplot(aes(sample = pageviews_est)) + 
  geom_qq() + 
  geom_qq_line() +
  theme_minimal()

# Q-Q plot (logged)
mpox_df |> 
  ggplot(aes(sample = log(pageviews_est))) + 
  geom_qq() + 
  geom_qq_line() +
  theme_minimal()

# Anderson-Darling Test
mpox_df |> 
  filter(country == "United States") |> 
  mutate(pageviews_logged = log(pageviews_est)) |> 
  pull(pageviews_logged) |> 
  ad.test()

# Anderson-Darling Test (logged)
ad.test(log(mpox_df$pageviews_est))

# Lilliefors (Kolmogorov-Smirnov Test) 
lillie.test(mpox_df$pageviews_est)

# Lilliefors (Kolmogorov-Smirnov Test) (logged)
lillie.test(log(mpox_df$pageviews_est))

# Shapiro-Wilk Test
shapiro.test(mpox_df$pageviews_est)

# Shapiro-Wilk Test (logged)
shapiro.test(log(mpox_df$pageviews_est))


# Mpox cases -------------------------------------------------------------------
# histogram
mpox_df |> 
  ggplot(aes(x = cases_moving_avg)) + 
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  theme_minimal()

# histogram (logged)
mpox_df |> 
  filter(cases_moving_avg >= 1) |> 
  ggplot(aes(x = log(cases_moving_avg))) +  # logged
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  theme_minimal()

# Q-Q plot 
mpox_df |>  
  ggplot(aes(sample = cases_moving_avg)) + 
  geom_qq() + 
  geom_qq_line() +
  theme_minimal()

# Q-Q plot (logged)
mpox_df |> 
  ggplot(aes(sample = log(cases_moving_avg))) + 
  geom_qq() + 
  geom_qq_line() +
  theme_minimal()


# Anderson-Darling Test 
mpox_df |> 
  filter(cases_moving_avg > 0) |> 
  pull(cases_moving_avg) |> 
  ad.test()

# Anderson-Darling Test (logged)
mpox_df |> 
  filter(cases_moving_avg > 0) |> 
  mutate(cases_moving_avg_logged = log(cases_moving_avg)) |> 
  pull(cases_moving_avg_logged) |> 
  ad.test()

# Lilliefors (Kolmogorov-Smirnov Test) 
lillie.test(mpox_df$cases_moving_avg)

# Lilliefors (Kolmogorov-Smirnov Test) (logged)
mpox_df |> 
  filter(cases_moving_avg > 0) |> 
  mutate(cases_moving_avg_logged = log(cases_moving_avg)) |> 
  pull(cases_moving_avg_logged) |> 
  lillie.test()

# Shapiro-Wilk Test
shapiro.test(mpox_df$cases_moving_avg)

# Shapiro-Wilk Test (logged)
mpox_df |> 
  filter(cases_moving_avg > 0) |> 
  mutate(cases_moving_avg_logged = log(cases_moving_avg)) |> 
  pull(cases_moving_avg_logged) |> 
  shapiro.test()

# Conclusion -------------------------------------------------------------------
#> According to all normality performed, the data fails to meet the assumption
#> that they are normally distribution, therefore the Pearson correlation 
#> coefficient is not appropriate for this case. Instead, the Spearman correlation
#> can be used.
