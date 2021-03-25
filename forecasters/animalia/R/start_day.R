#' Produce start_day function
#'
#' @param ahead vector of aheads (in days)
#' @param training_window_size desired size of the training set
#' @param lags vector (or list) of lags for features
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
                           roll_sum_len = 7,
                           cv = FALSE,
                           cv_type = c("forward","random"),
                           nfolds = 5){
  forecast_date <- lubridate::ymd(forecast_date)
  function(forecast_date){
    as.Date(forecast_date) - max(ahead) - training_window_size - 
      max(unlist(lags)) + 1 - roll_sum_len
  }
}
