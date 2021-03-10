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
#' @export ms.covariate
ms.covariate <-
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
  "base_covariates",
  "debug_folder",
  "forecast_date",
  "geo_type",
  "impute_last_3_responses",
  "incidence_period",
  "learner",
  "location_covariates",
  "log_response",
  "n_locations",
  "quantiles",
  "response",
  "roll_lags",
  "seed",
  "weeks_back"
)

# List of valid values for options.  Option names not listed here have no value restrictions.
# When invalid options are specified, the first valid option will be used in its place.
VALID_MODELING_OPTIONS_VALUES <- list(
  learner = c("linear")
)

#' Validate the modeling options, fixing invalid values where possible.
#'
#' @param modeling_options named list of modeling options
#' @param base_df signal data with column `time_value`
#' @return a named list
ms.validate_options <- function(modeling_options,
                                base_df) {
  if (is.na(modeling_options) || is.null(modeling_options)) {
    stop("Modeling options must be non-empty")
  }
  # Verify that the set of options exactly matches the expected set.
  unknown_options <- setdiff(names(modeling_options), VALID_MODELING_OPTIONS)
  if (length(unknown_options) > 0) {
    stop("Unknown options specified: ", unknown_options)
  }
  missing_options <- setdiff(VALID_MODELING_OPTIONS, names(modeling_options))
  if (length(missing_options) > 0) {
    stop("Missing options: ", missing_options)
  }

  # Fix invalid option values.
  for (option in names(VALID_MODELING_OPTIONS_VALUES)) {
    if (!(modeling_options[option] %in% VALID_MODELING_OPTIONS_VALUES[option])) {
      message("Invalid ", option, "=", modeling_options[option], ", defaulting to ",
              VALID_MODELING_OPTIONS_VALUES[option][1])
      modeling_options[option] <- VALID_MODELING_OPTIONS_VALUES[option][1]
    } 
  }

  # Don't allow `weeks_back` to extend beyond the earliest available data. 
  max_possible_weeks_back <- floor(
    (as.numeric(modeling_options$forecast_date - min(base_df[['time_value']])) / 7) - modeling_options$ahead + 1)
  if (modeling_options$weeks_back > max_possible_weeks_back) {
    message('User requested weeks_back=', modeling_options$weeks_back,
            ', but there is not enough data.  Setting instead to maximum ',
            'possible weeks_back=', max_possible_weeks_back)
    modeling_options$weeks_back <- max_possible_weeks_back
  }

  modeling_options
}
