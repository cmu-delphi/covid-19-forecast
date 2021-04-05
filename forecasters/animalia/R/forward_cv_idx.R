
# Helper script to determine train/validation splits for forward cross validation
#
#
# the first 4 inputs are internal to the production_forecaster
# nfolds / ntrain_cv are possibly additional named params in ...
# Together, these determine the validation sets for forward validation
# before being passed along to cv_quantile_lasso
#' @importFrom dplyr between
forward_cv_idx <- function(time_value,
                           train_end_date,
                           training_window_size,
                           ahead,
                           nfolds = NULL,
                           ntrain_cv = NULL){
  nfolds <- ifelse(is.null(nfolds), 5, nfolds)
  ntrain_cv <- ifelse(is.null(ntrain_cv), training_window_size, ntrain_cv)
  assert_that(nfolds > 0, msg = "nfolds must be NULL or non-negative.")
  assert_that(lubridate::is.Date(train_end_date))
  assert_that(ntrain_cv > 0, msg = "ntrain_cv must be NULL or non-negative.")
  assert_that(length(ahead) == 1)

  cv_period_size <- nfolds + ahead + ntrain_cv
  available_cv_times <- between(time_value,
                                train_end_date - cv_period_size + 1,
                                train_end_date)
  assert_that(length(available_cv_times) == cv_period_size,
              msg = paste(
                "not enough data is available for requested forward validation.",
                sprintf("You need at least %i rows but there are only %i",
                        cv_period_size, length(available_cv_times))))
  train_test_inds = list(train = vector(mode = "list", length = nfolds),
                         test = vector(mode = "list", length = nfolds))
  for (k in seq_along(nfolds)) {
    validation_forecast_date = train_end_date - nfolds + k
    train_test_inds$train[[k]] = which(between(
      available_cv_times,
      validation_forecast_date - (ahead + ntrain_cv) + 1,
      validation_forecast_date - ahead))
    train_test_inds$test[[k]] = which(
      available_cv_times == validation_forecast_date)
  }
  train_test_inds$fit = which(between(
    available_cv_times,
    train_end_date - training_window_size + 1,
    train_end_date))
  return(train_test_inds)
}
