
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
#' 
#'
#'
#' @return a function that takes in a data frame with the same columns, 
#'   but possibly fewer rows
#' @name featurizers
#' @importFrom dplyr group_by arrange slice_max ungroup summarise across
#' @importFrom dplyr left_join
#' @importFrom rlang .data
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
                                        n_locations = 200) {
  response_name = rlang::sym(
    paste0("value+0:", response_data_source, "_", response_signal))
  function(df) {
    locs <- df %>% 
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
