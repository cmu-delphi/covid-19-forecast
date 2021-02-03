#' @include transformation.R
NULL

#' Pseudo named tuple for covariates
#'
#' @param name name of covariate (must have match in giant rdata)
#' @param tr transformation function to use
#' @param lags the lags to use (0 for none)
#' @param do_rollsum bool to roll sum the covariate
#' @param do_rollavg bool to roll avg the covariate
#' @param do_penalize bool to penalize the covariate in the lasso
#' @return a named list
#' @export ds.covariate
ds.covariate <-
  function(name,
           tr = tr.identity,
           lags = c(0),
           do_rollsum = F,
           do_rollavg = F,
           do_penalize = T) {
    list(
      name = name,
      tr = tr,
      lags = lags,
      do_rollsum = do_rollsum,
      do_rollavg = do_rollavg,
      do_penalize = do_penalize
    )
  }

# new variables will have below suffixes appended
ROLLSUM_SUFFIX <- "rollsum"
ROLLAVG_SUFFIX <- "rollavg"
LAG_SUFFIX <- "lag"

VALID_MODELING_OPTIONS <- c(
  "ahead",
  "backfill_buffer",
  "cdc_probs",
  "debug_folder",
  "earliest_data_date",
  "forecast_date",
  "geo_type",
  "impute_last_3_response_covariate",
  "incidence_period",
  "learner",
  "location_covariates",
  "log_response",
  "model_covariates",
  "n_locations",
  "response",
  "roll_lags",
  "seed",
  "weeks_back"
)

MUTUAL_DEFAULTS <- list(
  ahead = 1,
  backfill_buffer = 5,
  cdc_probs = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99),
  debug_folder = NULL,
  incidence_period = "epiweek",
  location_covariates = list(
    ds.covariate("population", tr = tr.log_pad)
  ),
  log_response = TRUE,
  roll_lags = 7,
  seed = 2020,
  impute_last_3_response_covariate = T,
  weeks_back = Inf
)

STATE_DEFAULTS <- list(
  response = "jhu-csse_deaths_incidence_num",
  model_covariates = list(
    ds.covariate("jhu-csse_deaths_incidence_num", tr = tr.log_pad, lags = c(1:3), do_rollsum = T)
  )
)

COUNTY_DEFAULTS <- list(
  response = "usa-facts_deaths_incidence_num",
  model_covariates = list(
    ds.covariate("usa-facts_deaths_incidence_num", tr = tr.log_pad, lags = c(1:3), do_rollsum = T)
  )
)

STRATIFIED_LINEAR_DEFAULTS <- list(
  learner = "stratified_linear"
)

#' Sets a default value if not already set
#'
#' @param modeling_options named list
#' @param key option to be set
#' @param value option setting
#' @return a named list
ds.set_default <- function(modeling_options, key, value) {
  if (!(key %in% names(modeling_options))) {
    modeling_options[[key]] <- value
  }
  return(modeling_options)
}

#' Sets default values from a named list
#'
#' @param modeling_options named list
#' @param options_list named list of defaults
#' @return a named list
ds.set_default_list <- function(modeling_options, options_list) {
  for (k in names(options_list)) {
    modeling_options <- ds.set_default(modeling_options, k, options_list[[k]])
  }
  return(modeling_options)
}

#' Sets modeling defaults
#' 
#' This function takes a named list, containing user-specified modeling
#' options, and returns a named list, where specific modeling options that
#' were not explicitly set by the user are set to package defaults, as
#' defined in this file.  This function also verifies that only valid modeling
#' options (both keys and values) are passed in by the user, and if an invalid
#' modeling option is specified, an error will be raised.
#'
#' @param modeling_options named list
#' @return a named list
ds.set_modeling_defaults <- function(modeling_options) {
  if (is.na(modeling_options) || is.null(modeling_options)) {
    modeling_options = list()
  }
  # Verify that there are no illegal arguments
  if ((length(extra_opts <- lubridate::setdiff(
    names(modeling_options),
    VALID_MODELING_OPTIONS
  ))) > 0) {
    stop("Illegal arguments specified: ", extra_opts)
  }
  modeling_options <- ds.set_default(modeling_options,
                                     "learner", "stratified_linear")
  if (modeling_options$learner == "stratified_linear") {
    modeling_options <- ds.set_default_list(modeling_options,
                                            STRATIFIED_LINEAR_DEFAULTS)
  } else {
    stop("Illegal learner specified: ", modeling_options$learner)
  }
  if (modeling_options$geo_type == 'county') {
    modeling_options <- ds.set_default_list(modeling_options,
                                            COUNTY_DEFAULTS)
  } else if (modeling_options$geo_type == 'state') {
    modeling_options <- ds.set_default_list(modeling_options,
                                            STATE_DEFAULTS)
  }
  modeling_options <- ds.set_default_list(modeling_options,
                                          MUTUAL_DEFAULTS)
  max_possible_weeks_back <- floor(
    (as.numeric(modeling_options$forecast_date
      - modeling_options$earliest_data_date)
    / 7) - modeling_options$ahead + 1)
  if (modeling_options$weeks_back > max_possible_weeks_back) {
    message('User requested weeks_back=', modeling_options$weeks_back,
            ', but there is not enough data.  Setting instead to maximum ',
            'possible weeks_back=', max_possible_weeks_back)
    modeling_options$weeks_back <- max_possible_weeks_back
  }

  modeling_options
}
