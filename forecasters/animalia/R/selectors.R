#' Functions to create "selectors" 
#' 
#' These create functions that take in a dataframe in wide format and
#' return a vector of geo_values. These geo_values are the ones that we
#' want forecasts for. (We currently only use them for county case 
#' predictions.)
#' 
#' @param response_data_source The name of the response.
#' @param response_signal The name of the response signal.
#' @param n_locations The number of locations to predict.
#' 
#' @return A function that takes in a data frame in wide format and returns
#'   a vector of geo_values.
#' @name selectors
#' @importFrom dplyr group_by slice_max summarise
#' @importFrom rlang .data
#' @export
select_geo_top_n <- function(response_data_source = "jhu-csse",
                             response_signal = "confirmed_incidence_num",
                             n_locations = 200) {
  if (n_locations <= 0) stop("n_locations must be a positive integer")
  response_name = rlang::sym(
    paste0("value+0:", response_data_source, "_", response_signal))
  
  function(df) {
    geo_values <- df %>% 
      group_by(.data$geo_value) %>%
      summarise(tot = sum(!!response_name, na.rm = TRUE)) %>%
      slice_max(order_by = .data$tot, n = n_locations) %>%
      select(.data$geo_value) %>%
      pull()
    return(geo_values)
  }
}
