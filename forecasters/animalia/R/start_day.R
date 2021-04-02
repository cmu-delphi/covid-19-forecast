#' Produce start_day function
#' 
#' This closure can be used to determine the earliest date data needs to be 
#' pulled in order to have sufficient available data at prediction time.
#' It can potentially cut down the fitting time.
#'
#' @param ahead vector of aheads
#' @param training_window_size desired size of the training set
#' @param lags vector (or list) of lags for features
#' @template incidence_period-template
#' @param roll_sum_len if features need trailing sums, how far back do we need?
#'   Default is 7
#' @param cv will we be performing cv? 
#' @param cv_type forward cv or random, forward, requires more historical data
#' @param nfolds how many folds to use for CV 
#' @param ntrain_cv how much data do we train CV with? default is 
#'   `training_window_size` (different from iid CV)
#'
#' @return returns a function with a single argument: the forecast_date as a 
#'   string or Date object
#' @export
grab_start_day <- function(ahead, 
                           training_window_size,
                           lags, 
                           incidence_period,
                           roll_sum_len = 7,
                           cv = FALSE,
                           cv_type = c("forward","random"),
                           nfolds = 5,
                           ntrain_cv = training_window_size){
  function(forecast_date){
    forecast_date <- lubridate::ymd(forecast_date)
    ahead_in_days <- purrr::map_dbl(ahead,  ~evalcast::get_target_ahead(
      forecast_date, incidence_period, .x))
    return(
      as.Date(
        forecast_date) - max(ahead_in_days) - training_window_size - 
        max(unlist(lags)) + 1 - roll_sum_len -
        # if forward cv, we need to go back even more
        (cv && cv_type == "forward") * (max(ahead_in_days) + nfolds)
    )
  }
}

start_day_ar <- function(forecast_date){
  return(as.Date(forecast_date) - (max(ahead) + nfolds + max(ahead) + n + max(lags) + hotspot_lag) + 1)
}
