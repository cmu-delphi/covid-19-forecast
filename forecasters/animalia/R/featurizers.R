#' Functions to create "featurizers" 
#' 
#' Essentially, these both create functions that compute
#' weekly averages of daily data. The state version needs no arguments at the 
#' moment. For counties, we need the name of the response so that we can choose
#' the top n counties.
#' 
#' @param response_data_source the name of the response
#' @param response_signal the name of the response signal
#' @param n_locations the number of locations to predict
#' @param location_size_filter_lookback when summing county totals to determine
#'   the top \code{n_locations} counties, how many days back should we sum over?
#'   Defaults to all days available. If you specify this argument, you must also
#'   pass the forecast date.
#' @param forecast_date required for non null
#'   \code{location_size_filter_lookback} otherwise ignored
#' 
#'
#'
#' @return a function that takes in a data frame with the same columns, 
#'   but possibly fewer rows
#' @name featurizers
#' @importFrom dplyr filter group_by arrange mutate slice_max select pull ungroup summarise across left_join
#' @importFrom rlang .data !!
NULL

#' @export
#' @rdname featurizers
make_state_7dav_featurizer <- function() {
  function(df){
    df %>% 
      group_by(.data$geo_value) %>% # maybe easier with modeltools::slide_by_geo
      arrange(.data$time_value) %>%
      mutate(across(starts_with("value"), 
                    ~ RcppRoll::roll_meanr(.x, n = 7, na.rm = TRUE) * 7),
             across(starts_with("value"),
                    ~ zoo::na.locf(.x, na.rm = FALSE))) %>%
      ungroup()
  }
}

#' @rdname featurizers
#' @export
make_county_7dav_featurizer <- function(response_data_source = "jhu-csse",
                                        response_signal = "confirmed_incidence_num",
                                        n_locations = 200,
                                        location_size_filter_lookback = NULL,
                                        forecast_date = NULL) {
  response_name = rlang::sym(
    paste0("value+0:", response_data_source, "_", response_signal))
  if (!is.null(location_size_filter_lookback)) 
    assertthat::assert_that(
      !is.null(forecast_date),
      msg = paste0("When filtering counties for specific recent period with\n",
                   "a specified `size_filter_lookback`, `forecast_date`\n",
                   "must be present."))
  function(df) {
    first_day_available <- min(df$time_value)
    window_start_day <-
      if (is.null(location_size_filter_lookback)) {
        first_day_available
      } else {
        forecast_date - lubridate::duration(location_size_filter_lookback, "days")
      }
    if (window_start_day < first_day_available) {
      warning("You're trying to use a larger window to choose\n",
              "than you have data available. Using everything.")
      window_start_day <- first_day_available
    }
    locs <- df %>% 
      filter(.data$time_value >= window_start_day) %>%
      group_by(.data$geo_value) %>%
      summarise(tot = sum(!!response_name, na.rm = TRUE)) %>%
      slice_max(order_by = .data$tot, n = n_locations) %>%
      select(.data$geo_value) %>%
      pull()
    df <- df %>% 
      filter(.data$geo_value %in% locs) %>%
      group_by(.data$geo_value) %>% # maybe easier with modeltools::slide_by_geo
      arrange(.data$time_value) %>%
      mutate(across(starts_with("value"), 
                    ~ RcppRoll::roll_meanr(.x, n = 7, na.rm = TRUE) * 7),
             across(starts_with("value"),
                    ~ zoo::na.locf(.x, na.rm = FALSE))) %>%
      ungroup()
  }
}
