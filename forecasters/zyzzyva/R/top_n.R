#'
#' This function returns the n** locations which have observed the largest
#' values of the response, cumulatively over all dates.
#'
#' **: (if there are multiple locations with value equal to the nth location,
#' they are all included)
#'
#' @param df data frame with columns geo_value, time_value,
#'   variable_name, and value (and possibly others, which we will ignore)
#' @param response string indicating the name of the response
#' @param n how many locations should we pick? If it is greater than the total
#'   number of locations, than we pick all of them.
#'
#' @export tn.get_top_n_locations
tn.get_top_n_locations <- function(df, response, n){
  stopifnot(is.data.frame(df),
            is.character(response),
            is.numeric(n))
  stopifnot(c("geo_value",
              "time_value",
              "variable_name",
              "value") %in% names(df))
  stopifnot(response %in% df$variable_name)
  stopifnot(n > 0, n == round(n))
  df_use <- df %>% select(geo_value, time_value, variable_name, value)
  df_latest <- df_use %>%
    filter(variable_name == response) %>%
    group_by(geo_value, time_value) %>%
    top_n(n = 1, wt = time_value) %>% # NA only chosen if it’s all there is
    ungroup()
  top_n_locations <- df_latest %>%
    group_by(geo_value) %>%
    summarise(value = sum(value, na.rm = T)) %>% # treat NA values as 0
    ungroup() %>%
    top_n(n = !!n,wt = value) %>% # may not result in exactly n locations, due to ties.
    pull(geo_value)
  return(top_n_locations)
}
