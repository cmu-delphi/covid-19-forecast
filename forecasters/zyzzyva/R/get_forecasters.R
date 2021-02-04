#' @include main.R
NULL

#' Get an evalcast-compliant zyzzyva forecaster based on the provided arguments.
#'
#' @param n_locations the maximum number of locations to forecast, ordered by response value
#'   descending.  Forecasts all locations when NULL.
#' @return a named list, with each element of the list consisting of a
#'     forecaster function and type (one of `c("standalone",
#'     "ensemble")`).
#' @export get_forecasters
get_forecasters  <- function(backfill_buffer = 5,
                             debug_folder = NULL,
                             impute_last_3_responses = TRUE,
                             learner = "stratified_linear",
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

        covidcast_model_covariates <- c(list(ds.covariate(response,
                                                          tr = tr.log_pad,
                                                          lags = c(1, 2, seq(3,21,3)),
                                                          do_rollsum = T)),
                                        lapply(other_covariates,
                                               function(cov) ds.covariate(cov,
                                                                          lags = seq(3,28,7),
                                                                          do_rollsum = T)))
        location_covariates = lapply(location_covariates,
                                     function(x) ds.covariate(x, tr = tr.log_pad))

        modeling_options <- list(
            ahead = ahead,
            backfill_buffer = backfill_buffer,
            debug_folder = debug_folder,
            forecast_date = forecast_date,
            geo_type = geo_type,
            impute_last_3_responses = impute_last_3_responses,
            incidence_period = incidence_period,
            learner = learner,
            location_covariates = location_covariates,
            log_response = log_response,
            model_covariates = covidcast_model_covariates,
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
