


#' Format evalcast predictions_cards for submission to Reich Lab
#'
#' Each forecaster produces a data frame of predictions. It is easiest to just
#' `bind_rows()` and pass the result in to this function.
#'
#'
#' @param predictions_cards data frame of predictions of the type returned by
#'   evalcast. You must pass in a single df.
#' @param geo_values_to_filter character string or list thereof of geo_values to exclude from the
#'   final output.
#'
#' @return a tibble for conversion to csv
#' @export
#' @importFrom purrr map map_lgl map_int map2
#' @importFrom dplyr bind_rows filter
#' @importFrom assertthat assert_that
format_predictions_for_reichlab_submission <- function(predictions_cards,
                                                       geo_values_to_filter = NULL){

  # (A) Remove non-predictions
  assert_that(class(predictions_cards)[1] == "predictions_cards",
              msg = "Predictions were not created by evalcast.")
  assert_that(nrow(predictions_cards) > 0,
              msg = "All predictions are empty.")

  # (B) Check to make sure
  #   -- first, all predictions cards have the same forecast date
  assert_that(n_distinct(predictions_cards$forecast_date) == 1L,
              msg = "There are multiple forecast dates in these predictions.")
  #   -- Ensure that we have the necessary columns
  assert_that(all(
    c("ahead", "geo_value", "quantile", "value", "signal", "target_end_date",
      "incidence_period") %in% names(predictions_cards)),
    msg = "Predictions are somehow missing necessary columns.")

  assert_that(all(predictions_cards$signal %in%
                    c("confirmed_incidence_num", "deaths_incidence_num")),
              msg = "Predictions are for unsupported signals")

  assert_that(all(predictions_cards$incidence_period == "epiweek"),
              msg = "Currently only submitting epiweek forecasts")

  case_quants <- c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)

  reichlab_quantile <- predictions_cards %>%
    mutate(target = if_else(.data$signal == "confirmed_incidence_num",
                            paste0(.data$ahead, " wk ahead inc case"),
                            paste0(.data$ahead, " wk ahead inc death")),
           type = "quantile",
           quantile = round(.data$quantile, 3)) %>%
    filter(.data$signal == "deaths_incidence_num" |
             allowed_quantiles(.data$quantile, case_quants)) %>%
    select(location = .data$geo_value, .data$forecast_date, .data$quantile,
           .data$value, .data$target, .data$target_end_date, .data$type)

  # We continue to use the median as our point forecast. Change this if that
  # changes
  reichlab_point <- reichlab_quantile %>%
    filter(evalcast:::find_quantile_match(.data$quantile, 0.5)) %>%
    mutate(type = "point", quantile = NA)

  reichlab_df <- bind_rows(reichlab_quantile, reichlab_point) %>%
    filter(!.data$location %in% geo_values_to_filter)
  locs_to_reformat <- reichlab_df$location[nchar(reichlab_df$location) == 2L]
  reichlab_df$location[nchar(reichlab_df$location) == 2L] <-
    evalcast:::abbr_2_fips(locs_to_reformat)

  reichlab_df

}


allowed_quantiles <- function(x, q, tol = 1e-6) {
  apply(abs(outer(x, q, "-")) < tol, 1, any) %>% drop()
}

