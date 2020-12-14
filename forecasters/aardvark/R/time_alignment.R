#-----------------------------------------------------------------------------#
# This script contains aligners: functions used to compute aligned time.
# Each aligner should take as input:
#   df_use: a data frame, of the same form as the upstream df
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
#-----------------------------------------------------------------------------#

make_time_aligner <- function(alignment_variable, threshold, ahead, incidence_period = "epiweek"){
  # Closure, so that alignment functions can take standard input.
  # Inputs:
  #   alignment_variable: what variable to use for alignment?
  #   threshold: how large a value of the variable to treat as the threshold for start of the pandemic?
  #   ahead, incidence_period: canonical parameters used by the evaluator.
  days_since_threshold_attained_first_time_aligner <- function(df_use, forecast_date){
    # Compute aligned time as the number of days since a given variable attained a given threshold
    # for the first time.
    stopifnot(alignment_variable %in% unique(df_use %>% pull(variable_name)))
    
    # (1) Restrict ourselves to the data we need
    df_alignment_variable <- df_use %>% filter(variable_name == alignment_variable)
    
    # (2) Compute day0 --- the first date the threshold was attained for each location
    day0 <- df_alignment_variable %>% 
      filter(value >= threshold) %>%
      select(-value) %>%
      group_by(location) %>%
      summarise(value = min(time_value)) %>%
      ungroup()
    
    # (3) Compute days since variable crossed threshold
    #     If the threshold has not yet been reached for a given (location, time_value), 
    #     we assign a value of NA.

    ## (A) Create an empty data frame we wish to populate.
    ##     This makes sure we satisfy the aligner guarantee.
    locations <- df_use %>% pull(location) %>% unique
    train_dates <- df_use %>% pull(time_value) %>% unique
    target_dates <-  get_target_period(forecast_date, incidence_period, ahead) %$%
      seq(start, end, by = "days")
    dates <- unique(c(train_dates, target_dates))
    df_empty <- expand_grid(location = locations, time_value = dates)
    
    ## (B) Populate the empty data frame.
    df_align <- left_join(df_empty, day0, by = "location") %>%
      mutate(align_date = ifelse(time_value - value >= 0, time_value - value, NA)) %>%
      select(-value)
    
    return(df_align)
  }
}