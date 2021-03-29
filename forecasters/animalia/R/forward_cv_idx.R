# Helper script to determine train/validation splits for forward cross validation
#' @importFrom dplyr between
#' 
#' currently unimplemented
#' 
#' the first 4 inputs are internal to the production_forecaster
#' nfolds / ntrain are possibly additional named params in ...
#' Together, these determine the validation sets for forward validation
#' before being passed along to cv_quantile_lasso
forward_cv_idx <- function(time_value, 
                           train_end_date, 
                           training_window_size,
                           ahead,
                           nfolds,
                           ntrain){
  train_time_value <- between(time_value,
                              train_end_date - training_window_size + 1,
                              train_end_date) 
  nfolds <- ifelse(is.null(nfolds), 5, nfolds)
  nfolds <- ifelse(is.null(ntrain), training_window_size - nfolds, ntrain)
  train_test_inds <- list(train = vector(mode = "list", length = nfolds),
                          test = vector(mode = "list", length = nfolds))
  for (k in seq_len(nfolds)) {
    validation_forecast_date <- train_end_date - nfolds + k
    train_test_inds$train[[k]] <- which(
      between(train_time_value,
              validation_forecast_date - ahead - ntrain + 1,
              validation_forecast_date - ahead))
    train_test_inds$test[[k]] <- which(
      train_time_value == validation_forecast_date)
  }
  train_test_inds$fit <- which(
    between(train_time_value,
            train_end_date - ntrain + 1,
            train_end_date))
  return(train_test_inds)
}
