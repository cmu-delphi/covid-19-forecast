#' Get weekly averages of daily data
#'
#' @param df a "wide" data frame as created by [covidcast::aggregate_signals()].
#'   should contain the columns `geo_value` and `time_value` as well as columns
#'   containing leads and lags. These will all begin with the string "value"
#'
#' @return a data frame with the same columns, but possibly fewer rows
#' @export
#' @importFrom dplyr group_by arrange slice_max ungroup summarise across
#' @importFrom rlang .data
anteater_featurizer <- function(df) {
  df %>% 
    group_by(.data$geo_value) %>% # maybe easier with modeltools::slide_by_geo
    arrange(.data$time_value) %>%
    mutate(across(starts_with("value"), 
                  ~ RcppRoll::roll_meanr(.x, n = 7, na.rm = TRUE) * 7),
           across(starts_with("value"),
                  ~ zoo::na.locf(.x, na.rm = FALSE))) %>%
    ungroup()
}

#' @describeIn anteater_featurizer Selects top 200 locations by total cases before averaging
zebra_featurizer <- function(df) {
  locs <- df %>% 
    group_by(.data$geo_value) %>%
    summarise(tot = sum(.data$`value+0:jhu-csse_confirmed_incidence_num`)) %>%
    slice_max(order_by = .data$tot, n = 200) %>%
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