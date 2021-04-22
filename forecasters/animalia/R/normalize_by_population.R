#' Helper function for normalizing COVIDcast signals by population
#' 
#' Helper function to normalize COVIDcast signals by population. This replaces
#' the `value` column in each signal with the appropriately normalized
#' value. The user can choose which signals to normalize. Currently works with
#' "state" and "county" `geo_type` only.
#' 
#' @param df_list List of signal values to use for forecasting, of the format
#'   that is returned by [covidcast::covidcast_signals()].
#' @param norm_by_popn Should the response/features be normalized by population?
#'   This can be a single boolean value or a list of boolean values having the
#'   same length as the number of elements in the `df` list, which tells us
#'   which signals in `df` should be normalized by population and which
#'   should not. Default is `FALSE`, i.e. no normalization.
#' @param base The base for the normalization, i.e. the resulting values 
#' should be interpreted as `value` per `base`. Default is `1e5`.
#' 
#' @return `df_list` but with the `value` column appropriately normalized.
#' 
#' @importFrom dplyr mutate select
normalize_by_population <- function(df_list, norm_by_popn, base = 1e5) {
  nsigs <- length(df_list)
  # check that input is of the correct form
  if (!is.list(norm_by_popn)) {
    norm_by_popn <- rep(list(norm_by_popn), nsigs)
  } else {
    assert_that(
      length(norm_by_popn) == nsigs,
      msg = paste("If `norm_by_popn` is a list, it must have length equal",
                  "to the number of signals."))
  }
  
  for (i in seq_along(df_list)) {
    if (norm_by_popn[[i]]) {
      # get the correct set of population data based on the signal's metadata
      if (attr(df_list[[i]], "metadata")$geo_type == "state") {
        pop_data <- animalia::state_population
      } else if (attr(df_list[[i]], "metadata")$geo_type == "county") {
        pop_data <- animalia::county_population
      } else {
        stop(paste("Normalization by population currently works for state and",
                   "county `geo_type` only"))
      }
      pop_vec <- pop_data$population
      names(pop_vec) <- pop_data$geo_value
      
      df_list[[i]] <- df_list[[i]] %>%
        mutate(population = unname(pop_vec[geo_value])) %>%
        mutate(value = value / population * base) %>%
        select(-population)
    }
  }
  return(df_list)
}