#' Produce start_day function
#'
#' @param ahead vector of aheads
#' @param training_window_size desired size of the training set
#' @param lags vector (or list) of lags for features
#' @template incidence_period-template
#' @param roll_sum_len if features need trailing sums, how far back do we need?
#'   Default is 7
#' @param cv will we be performing cv? (currently ignored)
#' @param cv_type forward cv or random, forward, requires more historical data
#'   (currently ignored)
#' @param nfolds how many folds to use for CV (currently ignored)
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
                           nfolds = 5){
  function(forecast_date){
    forecast_date <- lubridate::ymd(forecast_date)
    ahead_in_days <- purrr::map_dbl(ahead,  ~evalcast::get_target_ahead(
      forecast_date, incidence_period, .x))
    return(
      as.Date(forecast_date) - max(ahead_in_days) - training_window_size - 
        max(unlist(lags)) + 1 - roll_sum_len
    )
  }
}
