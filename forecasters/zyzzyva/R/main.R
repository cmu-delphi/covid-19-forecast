#' @include data_structures.R io.R modeling.R preprocessing.R
NULL

#' Perform stacked forecasting
#'
#' @param base_df dataframe containing some covariate and response information
#' @param forecast_date date on which we start producing forecasts
#' @param n_locations the maximum number of locations to forecast, ordered by response value
#'   descending.  Forecasts all locations when NULL.
#' @param modeling_options a named list, additional elements of which
#'     overrides learner-dependent options set within the code
#' @return function that performs forecasting with proper options set
raw_forecaster <- function(base_df,
                           modeling_options) {
  set.seed(modeling_options$seed)
  modeling_options$earliest_data_date <- min(base_df[['time_value']])
  modeling_options <- ds.set_modeling_defaults(modeling_options)
  location_info_df <- io.load_location_info(modeling_options$geo_type)
  train_test <- pp.make_train_test(base_df,
                                   location_info_df,
                                   modeling_options)
  predicted_quantiles <- ml.fit_model(train_test, modeling_options)
  return(predicted_quantiles)
}
