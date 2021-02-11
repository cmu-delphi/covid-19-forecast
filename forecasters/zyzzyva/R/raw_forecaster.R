#' @include modeling_structure.R io.R modeling.R preprocessing.R
NULL

#' Perform stacked forecasting
#'
#' @param base_df dataframe containing some covariate and response information
#' @param modeling_options a named list, additional elements of which
#'     overrides learner-dependent options set within the code
#' @return function that performs forecasting with proper options set
raw_forecaster <- function(base_df,
                           modeling_options) {
  set.seed(modeling_options$seed)
  modeling_options <- ms.validate_options(modeling_options,
                                          base_df)
  location_info_df <- io.load_location_info(modeling_options$geo_type,
                                            modeling_options$location_covariates)
  train_test <- pp.make_train_test(base_df,
                                   location_info_df,
                                   modeling_options)
  predicted_quantiles <- ml.fit_model(train_test, modeling_options)
  return(predicted_quantiles)
}
