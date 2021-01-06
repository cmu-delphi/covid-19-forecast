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
#' @param response the response (e.g. "usafacts_deaths_incidence_num")
#' @param incidence_period the incidence period (e.g. "epiweek" for
#'     now, for all forecasters)
#' @param ahead the ahead parameter (e.g. 1, 2, 3, 4)
#' @param forecast_date the date of the forecast
#' @param geo_type the geographic type (e.g "county" or "state" or
#'     "hrr" or "msa"... but for now only the first two),
#' @param n_locations the number of locations (for now we will use 200
#'     for this)
#' @return a named list, with each element of the list consisting of a
#'     forecaster function and type (one of `c("standalone",
#'     "ensemble")`). Unavailable forecasters are marked as
#'     `list(forecaster = NA, type = "standalone")`.
#' @export get_forecasters
get_forecasters  <- function(response, incidence_period = c("epiweek"), ahead, forecast_date,
                             geo_type = c("county", "state", "hrr", "msa"),
                             n_locations = 200) {
    incidence_period <- match.arg(incidence_period)
    ## Currently we only work with "county" or "state" level forecasts
    geo_type <- match.arg(geo_type)

    ## For strawman, one function does everything and only variant is the version.
    ## It also doesn't use all the arguments we have available, so
    ##
    if (geo_type %in% c("county", "state")) {
        list(
            strawman =
                list(forecaster = strawman_forecaster(response = response, incidence_period = incidence_period,
                                                      ahead = ahead, version = 2),
                     type = "standalone"),
            strawman1 =
                list(forecaster = strawman_forecaster(response = response, incidence_period = incidence_period,
                                                      ahead = ahead, version = 1),
                     type = "standalone")
            )
    } else {
        list(strawman = list(forecaster = NA, type = "standalone"),
             strawman1 = list(forecaster = NA, type = "standalone"))
    }
}
