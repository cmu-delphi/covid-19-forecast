#' @include main.R
NULL

#' Get the list of forecasters and associated forecaster types
#' provided by this package.
#'
#' @description The evaluator will first call this function with the
#'     parameters shown below to determine all the forecasters
#'     available. It expects to get back a named list of lists of
#'     forecasting functions and types. If a forecasting function is
#'     not available for a given set of parameters, an `NA` should
#'     returned _instead of_ a function. This tells the evaluator to
#'     ignore that forecaster in a run: it ignores anything that is
#'     not a function. See examples in code below.
#'
#' @param n_locations the maximum number of locations to forecast, ordered by response value
#'   descending.  Forecasts all locations when NULL.
#' @return a named list, with each element of the list consisting of a
#'     forecaster function and type (one of `c("standalone",
#'     "ensemble")`).
#' @export get_forecasters
get_forecasters  <- function(n_locations = NULL,
                             weeks_back = 4) {
   forecaster_fn <- function(df,
                                  forecast_date,
                                  signals,
                                  incidence_period=c("epiweek"),
                                  ahead=1,
                                  geo_type=c("county", "state")) {
        incidence_period <- match.arg(incidence_period)
        geo_type <- match.arg(geo_type)

        signal_names <- paste(signals$data_source, signals$signal, sep="_")
        response <- signal_names[1]
        other_covariates <- signal_names[2:length(signal_names)]

        covidcast_model_covariates <- c(list(ds.covariate(response,
                                                        tr = tr.log_pad,
                                                        lags = c(1, 2, seq(3,21,3)),
                                                        do_rollsum = T)),
                                        lapply(other_covariates,
                                            function(cov) ds.covariate(cov,
                                                                        lags = seq(3,28,7),
                                                                        do_rollsum = T))
                                    )

        modeling_options <- list(
            ahead = ahead,
            forecast_date = forecast_date,
            geo_type = geo_type,
            impute_last_3_responses = TRUE,
            incidence_period = incidence_period,
            log_response = TRUE,
            learner = "stratified_linear",
            model_covariates = covidcast_model_covariates,
            n_locations = n_locations,
            quantiles = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99),
            response = response,
            seed = 2020,
            weeks_back = weeks_back
        )

        full_df <- bind_rows(df) %>%
        mutate(variable_name = paste(data_source, signal, sep="_")) %>%
        select(-c(lag, data_source, signal, stderr, sample_size, issue))

        raw_forecaster(
            full_df,
            modeling_options=modeling_options
        )
    }
    return(list(zyzzyva_covidcast = list(forecaster = forecaster_fn, type="standalone")))
}
