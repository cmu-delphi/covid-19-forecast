#' Functions to create "featurizers" 
#' 
#' Essentially, these create functions that compute
#' weekly averages of daily data. The function needs no arguments at the 
#' moment.
#'
#' @return A function that takes in a data frame with the same columns, 
#'   but possibly fewer rows.
#' @name featurizers
#' @importFrom dplyr group_by arrange ungroup across mutate
#' @importFrom rlang .data
NULL

#' @export
#' @rdname featurizers
make_7dav_featurizer <- function() {
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