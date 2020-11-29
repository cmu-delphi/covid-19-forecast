#--------------------------------------------------#
# This script contains aligners: functions used to compute aligned time.
# Each aligner should take as input:
#   df_use: a data frame, of the same form as the upstream_df
#   forecast_date: a canonical parameter used by the evaluator.
# and output a data frame
#   df_align: a data frame with columns location, time_value, and align_date.
# 
# Each aligner should satisfy the "aligner guarantee". 
# This guarantees that an aligned date is computed for
# 
# --every location at which data has been observed
# --every date at which data has been observed
# --every date in the target period, defined by the forecast_date.
#--------------------------------------------------#

make_days_since_threshold_attained_first_time_aligner <- function(variables, threshold, ahead){
  # Closure, so that alignment functions can take standard input.
  # Inputs:
  #   
  #   variables: what variable to use for alignment?
  # 
  #   threshold: how large a value of the variable to treat as the threshold for start of the pandemic?
  #  
  #   ahead, incidence_period: canonical parameters used by the evaluator.
  stopifnot(length(variables) == 1)
  days_since_threshold_attained_first_time_aligner <- function(df_use, forecast_date){
    # Compute aligned time as the number of days since a given variable attained a given threshold
    # for the first time.
    # Inputs:
    #   df_use, forecast_date: see top of this script.
    stopifnot(variables %in% unique(df_use %>% pull(variable_name))) # the variable we plan to align on better be in the data...
    
    # (1) Restrict ourselves to the data we need
    df_alignment_variable <- df_use %>%
      filter(variable_name == variables)
    stopifnot(all(!is.na(df_alignment_variable %>% pull(time_value)))) # alignment variable better be a temporal variable...
    
    # (2) Compute day0 --- the first date the threshold was attained --- for each location
    day0 <- df_alignment_variable %>% 
      filter(value >= threshold) %>%
      select(-value) %>%
      group_by(location) %>%
      summarise(value = min(time_value)) %>%
      ungroup()
    
    # (3) Compute days since variable 
    #     If the threshold has not yet been reached for a given (location,time_value), 
    #     we assign a value of NA.
    
    ## (A) Create an empty data frame we wish to populate.
    ##     This makes sure we satisfy the aligner guarantee.
    locations <- unique(df_use %>% pull(location))
    train_dates <- unique(df_use %>% pull(time_value) )
    target_dates <-  evalcast::get_target_period(forecast_date, "epiweek", ahead) %$%
      seq(start, end, by = "days")
    dates <- unique(c(train_dates, target_dates))
    df_empty <- expand_grid(location = locations,
                            time_value = dates)
    
    ## (B) Populate the empty data frame.
    df_align <- left_join(df_empty, day0, by = "location") %>%
      mutate(align_date = ifelse(time_value - value >= 0, time_value - value, NA)) %>%
      select(-value)
    
    return(df_align)
  }
}

make_days_since_threshold_crossed_most_recent_time_aligner <- function(variables, threshold, ahead,
                                                                       k, ave = rollmean){
  # Closure, so that alignment functions can take standard input.
  # Inputs:
  # 
  #   variables: what variable to use for alignment?
  #   threshold: how large a value of the variable to treat as the threshold for start of the pandemic?
  #
  #   k: how many days to smooth over
  #
  #   ave: function to use for smoothing. Default is rolling mean.
  #  
  #   ahead, incidence_period: canonical parameters used by the evaluator.
  stopifnot(length(variables) == 1)
  days_since_threshold_cross_most_recent_time_aligner <- function(df_use, forecast_date){
    # Compute aligned time as the number of days since a smoothed version
    # of a given variable crossed the threshold.
    # Inputs:
    #   df_use, forecast_date: see top of this script.
    stopifnot(variables %in% unique(df_use %>% pull(variable_name))) # the variable we plan to align on better be in the data...
    
    # (1) Restrict ourselves to the data we need
    df_alignment_variable <- df_use %>%
      filter(variable_name == variables)
    stopifnot(all(!is.na(df_alignment_variable %>% pull(time_value)))) # alignment variable better be a temporal variable...
    
    # (2) Smooth data.
    first_date <- min(df_alignment_variable %>% pull(time_value))
    last_date <- max(df_alignment_variable %>% pull(time_value))
    date_df <- data.frame(time_value = seq(first_date, last_date, by = "days"))
    full_df <- left_join(date_df, df_alignment_variable, by = c("time_value"))
    
    df_alignment_variable_smooth <- full_df %>% 
      group_by(location) %>%
      arrange(time_value) %>% 
      mutate(value = ave(value, k, align = "center", fill = "extend")) %>%
      ungroup
    
    # (3) Find all the dates on which the threshold was crossed for a given location.
    df_dates_above_threshold <- df_alignment_variable_smooth %>%
      select(location,time_value,value) %>%
      filter(value >= threshold) %>%
      select(-value)
    df_first_date <- expand_grid(location = unique(df_dates_above_threshold %>% pull(location)),
                                 time_value = first_date)
    df_dates_below_threshold <- df_alignment_variable_smooth %>%
      select(location,time_value,value) %>%
      filter(value < threshold) %>%
      mutate(time_value = time_value + 1) %>%
      select(-value) %>%
      bind_rows(df_first_date)
    day0 <- inner_join(df_dates_above_threshold,
                       df_dates_below_threshold, 
                       by = c('location','time_value')) %>%
      rename(value = time_value)
    
    # (3) Compute days since variable 
    #     If the threshold has never been reached for a given (location,time_value), 
    #     we assign a value of NA.
    
    ## (A) Create an empty data frame we wish to populate.
    ##     This makes sure we satisfy the aligner guarantee.
    locations <- unique(df_use %>% pull(location))
    train_dates <- unique(df_use %>% pull(time_value) )
    target_dates <-  evalcast::get_target_period(forecast_date,"epiweek",ahead) %$%
      seq(start,end,by = "days")
    dates <- unique(c(train_dates,target_dates))
    df_empty <- expand_grid(location = locations,
                            time_value = dates)
    
    ## (B) Populate the empty data frame.
    df_align <- left_join(df_empty, day0, by = "location") %>%
      mutate(align_date = ifelse(time_value - value >= 0, time_value - value, NA)) %>%
      group_by(location,time_value) %>%
      summarise(align_date = ifelse(all(is.na(align_date)), 
                                     NA,
                                     min(align_date,na.rm = T)))
    return(df_align)
  }
}