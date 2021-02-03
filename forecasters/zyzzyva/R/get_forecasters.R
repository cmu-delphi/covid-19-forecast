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
#' @param response the response (e.g. "usa-facts_deaths_incidence_num")
#' @param incidence_period the incidence period (e.g. "epiweek" for
#'     now, for all forecasters)
#' @param ahead the ahead parameter (e.g. 1, 2, 3, 4)
#' @param forecast_date the date of the forecast
#' @param geo_type the geographic type (e.g "county" or "state" or
#'     "hrr" or "msa"... but for now only the first two),
#' @param n_locations the maximum number of locations to forecast, ordered by response value
#'   descending.  Forecasts all locations when NULL.
#' @return a named list, with each element of the list consisting of a
#'     forecaster function and type (one of `c("standalone",
#'     "ensemble")`). Unavailable forecasters are marked as
#'     `list(forecaster = NA, type = "standalone")`.
#' @export get_forecasters
get_forecasters  <- function(geo_type,
                             n_locations=NULL) {
    ## Currently we only work with "county" or "state" level forecasts

    ## If your function has not been completed for geo_type == "state"
    ## say, you can return NA as the "else" part shows below.  Note
    ## also, how you can pass along additional parameters besides the
    ## six mandatory ones.

    if (geo_type %in% c("county", "state")) {
        list(
            zyzzyva_covidcast =
                list(forecaster=stacked_forecaster(
                        n_locations=n_locations),
                     type="standalone")
        )
    } else {
        list(zyzzyva =
                 list(forecaster=NA, type="standalone")
             )
    }
}
