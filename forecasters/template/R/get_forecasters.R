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
    ## Add checks to ensure that your forecaster can handle passed task parameters
    ## either in a separate function or here as shown below.

    ## Examples. Suppose you have five incarnations of your
    ## forecaster. All versions work off the same code base except
    ## with different values for some other variables besides those
    ## passed to this `get_forecasters` function, or maybe even a few
    ## other tweaks.
    ##
    ## For example:
    ## - incarnation 1 (the default) uses parameters `weeks_back = 3` and `solver = "gurobi"
    ## - incarnation 2 uses parameters `weeks_back = 2` and `solver = "gurobi"
    ## - incarnation 3 uses parameters `weeks_back = 3` and `solver = "Rglpk"
    ## - incarnation 4 uses parameters `weeks_back = 2` and `solver = "Rglpk"
    ## - incarnation 5 is an ensemble method that uses the results of all "standalone" forecasters with `solver = NA`
    ##
    ## Let's give codenames for these: `fisher`, `bayes`, `mle`, `mcmc`, and `ebayes` respectively.
    ## Then you should return a list such as this where `my_forecaster` is your forecaster function.
    ## list(
    ##     fisher = list(forecaster = my_forecaster(response = response, incidence_period = incidence_period,
    ##                                              ahead = ahead, forecast_date = forecast_date,
    ##                                              geo_type = geo_type, n_locations = n_locations,
    ##                                              weeks_back = 3, solver = "gurobi"),
    ##                   type = "standalone"),
    ##     bayes = list(forecaster = my_forecaster(response = response, incidence_period = incidence_period,
    ##                                             ahead = ahead, forecast_date = forecast_date,
    ##                                             geo_type = geo_type, n_locations = n_locations,
    ##                                             weeks_back = 2, solver = "gurobi"),
    ##                  type = "standalone"),
    ##     mle = list(forecaster = my_forecaster(response = response, incidence_period = incidence_period,
    ##                                           ahead = ahead, forecast_date = forecast_date,
    ##                                           geo_type = geo_type, n_locations = n_locations,
    ##                                           weeks_back = 2, solver = "Rglpk"),
    ##                  type = "standalone"),
    ##     mcmc = list(forecaster = my_forecaster(response = response, incidence_period = incidence_period,
    ##                                            ahead = ahead, forecast_date = forecast_date,
    ##                                            geo_type = geo_type, n_locations = n_locations,
    ##                                            weeks_back = 3, solver = "Rglpk"),
    ##                  type = "standalone"),
    ##     ebayes = list(forecaster = my_forecaster(response = response, incidence_period = incidence_period,
    ##                                              ahead = ahead, forecast_date = forecast_date,
    ##                                              geo_type = geo_type, n_locations = n_locations,
    ##                                              weeks_back = 3, solver = NA),
    ##                    type = "ensemble")
    ## )
    ##
    ## More sophisticated checks are possible. You may have some
    ## forecasters that only work for certain `ahead`s; e.g., `bayes`
    ## only works if only if `(ahead > 1 && ahead < 4)`. The the entry
    ## for `bayes` can be modified to not return a `function` thus:
    ##
    ## bayes = if (ahead <= 1 || ahead >= 4) {
    ##            list(forecaster = NA, type = "standalone") ## forecaster not a function!
    ##          } else {
    ##            list(forecaster = my_forecaster(response = response, incidence_period = incidence_period,
    ##                                            ahead = ahead, forecast_date = forecast_date,
    ##                                            geo_type = geo_type, n_locations = n_locations,
    ##                                            weeks_back = 2, solver = "gurobi"),
    ##                 type = "standalone")
    ##          }
    ##
    ## For strawman, one function does everything and only variant is the version.
    ## It also doesn't use all the arguments we have available, so
    ##
    ## list(
    ##     strawman = list(
    ##         forecaster = strawman_forecaster(response = response, incidence_period = incidence_period,
    ##                                          ahead = ahead, version = 2),
    ##         type = "standalone"),
    ##     strawman1 = list(
    ##         forecaster = strawman_forecaster(response = response, incidence_period = incidence_period,
    ##                                          ahead = ahead, version = 1),
    ##         type = "standalone")
    ## )
}

