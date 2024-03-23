# TODO: Streamline agg and ll plots
# TODO: Document helper funcs
# TODO: Specify namespace when using funcs

#' function to plot epidemic curves and number of deaths over time
vis_epicurve <- function(
    dat = NULL,
    data_type = "agg", # "ll"
    measure = "cases", # "deaths"
    frequency = "weekly", # "daily"
    start_date = as_date("2022-01-01"),
    end_date = max(dat$date),
    title = NULL,
    subtitle = NULL,
    fill_by = NULL,
    facet_by = NULL
    ) {

if (is.null(dat)) return("No dataset provided.")
  
if (frequency == "weekly") {
  dat <- dat |> 
    mutate(date = lubridate::floor_date(date, unit = "weeks", week_start = 3)) 
}

# aggregate data
if (data_type == "agg") {
  out <- dat |> 
    group_by(date, across(one_of(fill_by))) |> 
    #reframe(across(c(cases, deaths), ~sum(., na.rm = TRUE))) |> 
    # TODO: find alternative to aes_string() (deprecated)
    ggplot(aes_string(x = "date", y = measure, fill = fill_by)) +
    geom_col(col = "black", width = ifelse(frequency == "weekly", 7, 1)) +
    expand_limits(y = 5) +
    scale_x_date(
      limits = c(start_date, end_date), 
      expand = expansion(mult = 0.05),
      date_breaks = "3 months",
      date_labels = "%d %b\n%Y" 
    ) + 
    scale_y_continuous(
      limits = c(0, NA), 
      expand = expansion(mult = c(0.02, 0.02)), 
      breaks = pretty_breaks(),
      labels = comma_format()
    ) +
    labs(
      x = ifelse(frequency == "daily", "Date reported", "Week reported"),
      y = glue("Number of {measure}"),
      title = title,
      subtitle = subtitle,
      caption = "Source: WHO",
      fill = NULL
    ) +
    theme_minimal() + 
    theme(
      legend.position = "none",
      plot.margin = unit(c(0.2, 1, 0.2, 0.2), units = "cm")
    )
  
# line list data
} else if (data_type == "ll") {
  out <- dat |> 
    group_by(date, date_type, across(any_of(fill_by))) |> 
    reframe(cases = sum(cases, na.rm = TRUE)) |> 
    ggplot(aes(x = date, y = cases, fill = date_type)) +
    geom_bar(col = "black", width = ifelse(frequency == "weekly", 7, 1), stat = "identity", position = position_stack(reverse = TRUE)) +
    expand_limits(y = 5) +
    scale_x_date(
      limits = c(start_date, end_date), 
      expand = expansion(mult = 0.05),
      date_breaks = "3 months",
      date_labels = "%d %b\n%Y" 
    ) + 
    scale_y_continuous(
      limits = c(0, NA), 
      expand = expansion(mult = c(0.02, 0.02)), 
      breaks = pretty_breaks(),
      labels = comma_format()
    ) +
    labs(
      x = ifelse(frequency == "daily", "Date", "Week"),
      y = glue("Number of {measure}"),
      title = title,
      subtitle = subtitle,
      caption = "Source: WHO",
      fill = NULL
    ) +
    theme_minimal() + 
    theme(
      legend.position = "bottom",
      plot.margin = unit(c(0.2, 1, 0.2, 0.2), units = "cm")
    )
  
}

if (!is.null(facet_by)) {
  out <- out +
    facet_wrap(as.formula(paste0("~", facet_by)), ncol = 1, scales = "free_y") 
}

return(out)
  
}


