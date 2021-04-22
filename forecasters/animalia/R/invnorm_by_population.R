#' Helper function for undoing normalization by population
#' 
#' Helper function to undo normalization by population. This is useful when
#' we have made predictions for a response on a population-normalized basis,
#' and we want to convert them to predictions for the unnormalized response.
#' The function replaces the `value` column in the predictions data frame with
#' the unnormalized value. Currently works with "state" and "county" 
#' `geo_type` only.
#' 
#' @param df Data frame. Must include `geo_value` and `value` columns.
#' @param geo_type The geography type for which to request this data, either
#' "county" or "state".
#' @param base The base for the normalization, i.e. the resulting values 
#' should be interpreted as `value` per `base`. Default is `1e5`.
#' 
#' @return `df` but with the `value` column appropriately unnormalized.
#' 
#' @importFrom dplyr mutate select
invnorm_by_population <- function(df, geo_type, base = 1e5) {
  # get the correct set of population data
  if (geo_type == "state") {
    pop_data <- animalia::state_population
  } else if (geo_type == "county") {
    pop_data <- animalia::county_population
  } else {
    stop(paste("Undoing population normalization currently works for state and",
               "county `geo_type` only"))
  }
  pop_vec <- pop_data$population
  names(pop_vec) <- pop_data$geo_value
  
  df <- df %>%
    mutate(population = unname(pop_vec[geo_value])) %>%
    mutate(value = value / base * population) %>%
    select(-population)
  
  return(df)
}