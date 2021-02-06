#' @include raw_forecaster.R
NULL

#' Get an evalcast-compliant zyzzyva forecaster based on the provided arguments.
#'
#' @param debug_folder file path to which to write debug information.  When NULL, no debug
#'     information is written.
#' @param impute_last_3_responses whether to use an ets model to impute the most recent 3 days of
#'     the response variable.
#' @param learner name of learner to use
#' @param location_covariates list of names location covariates to use in model
#' @param log_response whether to log-pad the response
#' @param n_locations the maximum number of locations to forecast, ordered by response value
#'     descending.  Forecasts all locations when NULL.
#' @param quantiles list of quantile values at which to forecast
#' @param roll_lags lag time in days for applying rolling sums of covariates
#' @param seed seed for randomness
#' @param weeks_back number of weeks back of data to use in training.  Automatically adjusted to
#'     prevent avoid days not present in the data.
#' @return a named list, with each element of the list consisting of a
#'     forecaster function and type (one of `c("standalone",
#'     "ensemble")`).
#' @export get_forecasters
get_forecasters  <- function(debug_folder = NULL,
                             impute_last_3_responses = TRUE,
                             learner = "linear",
                             location_covariates = c("population"),
                             log_response = TRUE,
                             n_locations = NULL,
                             quantiles = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99),
                             roll_lags = 7,
                             seed = 2020,
                             weeks_back = Inf) {
    forecaster_fn <- function(df,
                              forecast_date,
                              signals,
                              incidence_period=c("epiweek"),
                              ahead=1,
                              geo_type=c("county", "state")) {
        incidence_period <- match.arg(incidence_period)
        geo_type <- match.arg(geo_type)

        base_df <- bind_rows(df) %>%
            mutate(variable_name = paste(data_source, signal, sep="_")) %>%
            select(geo_value, time_value, value, variable_name)

        signal_names <- paste(signals$data_source, signals$signal, sep="_")
        response <- signal_names[1]
        other_covariates <- signal_names[2:length(signal_names)]

        base_covariates <- c(list(ms.covariate(response,
                                               tr = tr.log_pad,
                                               lags = c(1, 2, seq(3,21,3)),
                                               do_rollsum = T)),
                             lapply(other_covariates,
                                    function(cov) ms.covariate(cov,
                                                               lags = seq(3,28,7),
                                                               do_rollsum = T)))
        location_covariates = lapply(location_covariates,
                                     function(x) ms.covariate(x, tr = tr.log_pad))

        modeling_options <- list(
            ahead = ahead,
            base_covariates = base_covariates,
            debug_folder = debug_folder,
            forecast_date = forecast_date,
            geo_type = geo_type,
            impute_last_3_responses = impute_last_3_responses,
            incidence_period = incidence_period,
            learner = learner,
            location_covariates = location_covariates,
            log_response = log_response,
            n_locations = n_locations,
            quantiles = quantiles,
            response = response,
            roll_lags = roll_lags,
            seed = seed,
            weeks_back = weeks_back
        )

        raw_forecaster(
            base_df,
            modeling_options=modeling_options
        )
    }
    return(list(zyzzyva_covidcast = list(forecaster = forecaster_fn, type="standalone")))
}
