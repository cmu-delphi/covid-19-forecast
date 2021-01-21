make_time_aligner <- function(alignment_variable, threshold, ahead, incidence_period = "epiweek"){

  days_since_threshold_attained_first_time_aligner <- function(df_use, forecast_date){

    stopifnot(alignment_variable %in% unique(df_use %>% pull(variable_name)))
    df_alignment_variable <- df_use %>% filter(variable_name == alignment_variable)
    
    day0 <- df_alignment_variable %>% 
      filter(value >= threshold) %>%
      select(-value) %>%
      group_by(location) %>%
      summarise(value = min(time_value), .groups = "drop")

    locations <- df_use %>% pull(location) %>% unique
    train_dates <- df_use %>% pull(time_value) %>% unique
    target_dates <-  get_target_period(forecast_date, incidence_period, ahead) %$%
      seq(start, end, by = "days")
    dates <- unique(c(train_dates, target_dates))
    df_align <- expand_grid(location = locations, time_value = dates) %>% 
      left_join(day0, by = "location") %>%
      mutate(align_date = ifelse(time_value - value >= 0, time_value - value, NA)) %>%
      select(-value)
    
    return(df_align)
  }
}
