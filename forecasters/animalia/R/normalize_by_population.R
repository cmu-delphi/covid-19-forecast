#' Helper function for normalizing COVIDcast signals by population
#' 
#' Helper function to either normalize or undo normalization of values by 
#' population. This replaces the `value` column in each data frame with the 
#' appropriately normalized (or unnormalized) value. The user can choose 
#' which data frames in the list to normalize. Currently works with
#' "state" and "county" `geo_type` only.
#' 
#' There are two main use cases for this function: (i) normalizing signals 
#' obtained from [covidcast::covidcast_signals()] by population, and
#' (ii) predictions for a response were made on a population-normalized basis
#' and we want to convert them to predictions for the unnormalized response.
#' 
#' @param df_list List of data frames, or a single data frame.
#'   All that is required is for each data frame in the list to have the 
#'   `geo_value` and `value` columns. (For example, the object returned from
#'   [covidcast::covidcast_signals()] satisfies this.)
#' @param geo_type The geography type associated with the data, either 
#'   "county" or "state". This should be a vector of values having the
#'   same length as the number of elements in the `df` list, indicating the
#'   `geo_type` associated with each signal. Alternatively, it can be a single
#'   character string if all the signals have the same `geo_type`.
#' @param signals_to_normalize Should the values be normalized by population?
#'   This can be a single boolean value or a vector of boolean values having the
#'   same length as the number of elements in the `df` list, which tells us
#'   which signals in `df` should be normalized by population and which
#'   should not. Default is `FALSE`, i.e. no normalization.
#' @param base The base for the normalization, i.e. the resulting values 
#' should be interpreted as `value` per `base`. Default is `1e5`.
#' @param invert If `FALSE` (default), we normalize by population. If `TRUE`,
#' we unnormalize (i.e. undo the normalization by population).
#' 
#' @return `df_list` but with the `value` column appropriately normalized
#' (or unnormalized).
#' 
#' @importFrom dplyr mutate select
#' @export
normalize_by_population <- function(df_list, 
                                    geo_type, 
                                    signals_to_normalize = FALSE, 
                                    base = 1e5,
                                    invert = FALSE) {
  if (class(df_list)[1] != "list") df_list <- list(df_list)
  nsigs <- length(df_list)
  # check that input is of the correct form
  if (length(signals_to_normalize) == 1) {
    signals_to_normalize <- rep(signals_to_normalize, nsigs)
  } else {
    assert_that(
      length(signals_to_normalize) == nsigs,
      msg = paste("If `signals_to_normalize` is a vector, it must have length equal",
                  "to the number of signals."))
  }
  if (length(geo_type) == 1) {
    geo_type <- rep(geo_type, nsigs)
  } else {
    assert_that(
      length(geo_type) == nsigs,
      msg = paste("If `geo_type` is a vector, it must have length equal",
                  "to the number of signals."))
  }
  
  for (i in seq_along(df_list)) {
    if (signals_to_normalize[i]) {
      # get the correct set of population data based on the signal's metadata
      if (geo_type[i] == "state") {
        pop_data <- animalia::state_population
      } else if (geo_type[i] == "county") {
        pop_data <- animalia::county_population
      } else {
        stop(paste("Normalization by population currently works for state and",
                   "county `geo_type` only"))
      }
      pop_vec <- pop_data$population
      names(pop_vec) <- pop_data$geo_value
      
      # the dplyr verbs seem to drop attributes, so we have to keep a copy
      # and restore them
      df_attributes <- attributes(df_list[[i]])
      if (!invert) {  # normalize
        df_list[[i]] <- df_list[[i]] %>%
          mutate(population = unname(pop_vec[geo_value])) %>%
          mutate(value = value / population * base) %>%
          select(-population)
      } else {        # unnormalize
        df_list[[i]] <- df_list[[i]] %>%
          mutate(population = unname(pop_vec[geo_value])) %>%
          mutate(value = value / base * population) %>%
          select(-population)
      }
      attributes(df_list[[i]]) <- df_attributes
    }
  }
  
  if (length(df_list) == 1) {
    return(df_list[[1]])
  } else {
    return(df_list)
  }
}
