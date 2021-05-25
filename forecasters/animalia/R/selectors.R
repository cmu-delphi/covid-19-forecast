#' Function factory for creating "selector" functions based on top response 
#' count
#' 
#' This creates a function that takes in a dataframe in list format and
#' returns a vector of geo_values. These geo_values are the ones that have the
#' largest cumulative value of the response, and are the ones that we
#' want forecasts for. (We currently only use them for county case 
#' predictions.)
#' 
#' @param n_locations The number of locations to predict/return.
#' 
#' @return A function that takes in a data frame in list format and returns
#'   a vector of geo_values.
#' @name selectors
#' @importFrom dplyr group_by slice_max summarise
#' @importFrom rlang .data
#' @export
select_geo_top_n <- function(n_locations = 200) {
  if (n_locations <= 0) stop("n_locations must be a positive integer")
  
  function(df_list) {
    if (class(df_list)[1] != "list") df_list <- list(df_list)
    
    geo_values <- df_list[[1]] %>% 
      group_by(.data$geo_value) %>%
      summarise(tot = sum(value, na.rm = TRUE)) %>%
      slice_max(order_by = .data$tot, n = n_locations) %>%
      select(.data$geo_value) %>%
      pull()
    return(geo_values)
  }
}
