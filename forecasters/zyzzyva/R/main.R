#' @include data_structures.R io.R modeling.R preprocessing.R
NULL

#' Wrapper function to create a forecasting function
#'
#' @param forecast_date the date of the forecast
#' @param n_locations the maximum number of locations to forecast, ordered by response value
#'   descending.  Forecasts all locations when NULL.
#' @param modeling_options a named list, additional elements of which
#'     overrides learner-dependent options set within the code
#' @return function that performs forecasting with proper options set
stacked_forecaster <- function(forecast_date,
                               n_locations=NULL,
                               modeling_options = list(
                                  learner='stratified_linear',
                                  weeks_back=4)) {
  function(df,
           forecast_date,
           signals,
           incidence_period=c("epiweek"),
           ahead=1,
           geo_type=c("county", "state")) {
    incidence_period <- match.arg(incidence_period)
    geo_type <- match.arg(geo_type)

    modeling_options$ahead <- ahead
    modeling_options$incidence_period <- incidence_period
    modeling_options$forecast_date <- forecast_date
    modeling_options$geo_type <- geo_type

    full_df <- bind_rows(df) %>%
      mutate(variable_name = paste(data_source, signal, sep="_")) %>%
      select(-c(lag, data_source, signal, stderr, sample_size, issue))

    raw_forecaster(
      full_df,
      forecast_date,
      n_locations,
      modeling_options=modeling_options
    )
  }
}

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
                           forecast_date,
                           n_locations,
                           modeling_options) {
  if (!is.null(n_locations)) {
    top_locs <- tn.get_top_n_locations(base_df, modeling_options$response, n_locations)
    base_df <- base_df %>% filter(geo_value %in% top_locs)
  }
  set.seed(modeling_options$seed)
  modeling_options$earliest_data_date <- min(base_df[['time_value']])
  modeling_options <- ds.set_modeling_defaults(modeling_options)
  location_info_df <- io.load_location_info(modeling_options$geo_type)
  train_test <- pp.make_train_test(base_df,
                                   location_info_df,
                                   forecast_date,
                                   n_locations,
                                   modeling_options)
  predicted_quantiles <- ml.fit_model(train_test, modeling_options)
  return(predicted_quantiles)
}
