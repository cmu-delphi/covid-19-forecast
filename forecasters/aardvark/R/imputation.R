make_mean_smoother <- function(k, first_date = NULL, last_date = NULL, ave = rollmean){
  # Closure to make a smoother.
  # Inputs:
  # -- k: number of days to take an average over
  # -- first_date: Date object, the first time_value on which we should see variables.
  # -- last_date: Date object, the last time_value on which we should see variables.

  mean_smoother <- function(dat){

    if (is.null(first_date)){
      first_date <- min(dat %>% pull(time_value))
    }
    if (is.null(last_date)){
      last_date <- max(dat %>% pull(time_value))
    }
    date_df <- data.frame(time_value = seq(first_date, last_date, by = "days"))
    full_df <- left_join(date_df, dat, by = c("time_value"))

    smoothed_dat <- full_df %>%
      group_by(location) %>%
      arrange(time_value) %>%
      mutate(smoothed_value = ave(value, k, align = "right", fill = "extend")) %>%
      ungroup
    return(smoothed_dat)
  }
}
