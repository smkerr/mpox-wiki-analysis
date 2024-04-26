# Check Residuals ==============================================================
plot_residuals <- function(model) {
  par(mfrow = c(2,2))
  plot(model, which = 1) # Residual vs Fitted
  plot(models$mpox, which = 2) # Q-Q Residuals
  plot(model, which = 3) # Scale-Location
  hist(resid(model)) # Histogram of residuals
}

# Print diagnostics
for (model in models) {
  plot_residuals(model)
}

train_df |> 
  ggplot(aes(date, Mpox)) +
  geom_line()
train_df |> 
  ggplot(aes(roll_cases, Mpox)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)
train_df |> 
  ggplot(aes(log(roll_cases), Mpox)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)
train_df |> 
  ggplot(aes(roll_cases, log(Mpox))) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)
train_df |> 
  ggplot(aes(log(roll_cases), log(Mpox))) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)
train_df |> 
  ggplot(aes(Mpox)) +
  geom_histogram() 
train_df |> 
  ggplot(aes(log(Mpox))) +
  geom_histogram() 
train_df |> 
  ggplot(aes((roll_cases))) +
  geom_histogram() 
train_df |> 
  ggplot(aes(log(roll_cases))) +
  geom_histogram() 