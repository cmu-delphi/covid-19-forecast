


#' Format evalcast predictions_cards for submission to Reich Lab
#'
#' @param predictions_cards list of predictions cards of the type returned by evalcast
#'
#' @return a tibble for conversion to csv
#' @export
#' @examples
#' baby_predictions = get_predictions(
#'   baseline_forecaster, "baby",
#'   tibble::tibble(
#'     data_source=c("jhu-csse", "usa-facts"),
#'     signal = c("deaths_incidence_num","confirmed_incidence_num"),
#'     start_day=lubridate::ymd("2020-09-15")),
#'   lubridate::ymd("2020-10-01"),
#'   "epiweek", ahead = 1:4, "state", "mi")
#' long_tbl = format_predictions_cards_for_reichlab_submission(baby_predictions)
#' @importFrom purrr map map_lgl map_int map2
#' @importFrom dplyr bind_rows filter
#' @importFrom assertthat assert_that
format_predictions_cards_for_reichlab_submission <- function(predictions_cards)
{

  ## (A) Remove non-predictions
  predictions_cards =
    predictions_cards[!is.na(predictions_cards)]
  predictions_cards =
    predictions_cards[!(predictions_cards %>% map_int(nrow) == 0)]
  assertthat::assert_that(length(predictions_cards) > 0,
                          msg = "All prediction cards are either NA or empty.")

  ## (B) Check to make sure
  ##   -- first, all predictions cards have the same forecast date
  ##   -- second, all predictions cards are for different forecasting tasks
  attribs <- purrr::map(predictions_cards, attributes)
  assertthat::assert_that(purrr::map_lgl(attribs, ~ length(.x) > 0) %>% all,
    msg = "At least one predictions_card has no attributes.")
  task_params <- c("signals", "incidence_period", "ahead", "geo_type",
                   "geo_values", "forecast_date")
  assertthat::assert_that(
    attribs %>% purrr::map_lgl(~ all(task_params %in% names(.x))) %>% all,
    msg = "At least one predictions_card is missing task information.")
  # prediction_params <- purrr::map(attribs, ~.x[task_params])
  # response_names <- purrr::map(prediction_params, ~.x["signals"]) %>%
  #  purrr::map(~dplyr::pull(.x[[1]][1,"signal"]))
  # prediction_params <- purrr::map(prediction_params, ~purrr::list_modify(.x, signals=NULL)) %>%
  #   purrr::map2(response_names, append)



  forecast_dates <- do.call("c", attribs %>% purrr::map("forecast_date"))
  assertthat::assert_that(
    length(unique(forecast_dates)) == 1,
    msg = "Each prediction card must be for the same forecast date.")
  # assertthat::assert_that(
  #   length(unique(params)) == length(params),
  #   msg = "Each prediction card must be for a separate forecasting task.")
  #   Not sure what this line was supposed to do. The message seems counter
  #   to the logical check. It seems that the check is ALWAYS true because
  #   params will be a list with length the number of aheads.
  #   See:
  #   https://github.com/cmu-delphi/covidcast-forecast/blob/362b7c1a65b0d2fbe5f47d91b16d13b1e5a52efe/forecaster_code/pipeline_code/common_funs/format.R#L53

  # (2) Reformat each prediction card
  reichlab_prediction_cards <- purrr::map(
    predictions_cards,
    ~format_prediction_card_for_reichlab_submission(.x))

  # (3) Combine all prediction cards into a single data frame with an additional
  #     column called forecast_date.

  reichlab_predicted <- dplyr::bind_rows(reichlab_prediction_cards)
  reichlab_predicted
}




#' @importFrom dplyr mutate rename pull select filter bind_rows
#' @importFrom tibble tibble
#' @importFrom tidyr expand_grid unnest
#' @importFrom evalcast get_target_period
format_prediction_card_for_reichlab_submission <- function(prediction_card)
{
  # Inputs:
  #
  #   prediction_card: A predictions card is created by the
  #    function get_predictions_card.

  # (1) Isolate necessary parameters

  response <- dplyr::pull(attributes(prediction_card)$signals[1,2])
  ahead <- attributes(prediction_card)$ahead
  incidence_period <- attributes(prediction_card)$incidence_period
  forecast_date <- attributes(prediction_card)$forecast_date

  # (2) Put quantiles into properly formatted data frame.
  param_df <-  tibble::tibble(
    target = rename_response_for_reichlab(response,ahead,incidence_period),
    target_end_date = evalcast::get_target_period(forecast_date,incidence_period,ahead)[["end"]],
  )
  quantile_param_df <- dplyr::mutate(param_df, type = "quantile")
  reichlab_quantile_df <- prediction_card %>%
    tidyr::unnest(cols = forecast_distribution) %>%
    dplyr::rename(quantile = probs, value = quantiles) %>%
    dplyr::mutate(quantile = round(quantile, 3)) %>%
    tidyr::expand_grid(quantile_param_df)

  # (3) Put point predictions into properly formatted data frame
  #     NOTE: We will use the median as our point predictions.
  point_param_df <- dplyr::mutate(param_df, type = "point")
  reichlab_point_pdf <- prediction_card %>%
    tidyr::unnest(cols = forecast_distribution) %>%
    dplyr::filter(abs(probs - .5) < 1e-8) %>% # restrict ourselves to the median
    dplyr::select(-probs) %>%
    dplyr::rename(value = quantiles) %>%
    dplyr::mutate(quantile = NA) %>%
    tidyr::expand_grid(point_param_df)

  out = dplyr::bind_rows(reichlab_quantile_df,reichlab_point_pdf)

  # if the predictions are case level then reduce quantiles to allowed
  if(response == "confirmed_incidence_num"){
    quant_allowed <- c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)
    out <- out %>%
      dplyr::filter(quantile %in% quant_allowed | type == "point")
  }
  out
}

#' @importFrom assertthat assert_that
rename_response_for_reichlab <- function(response,ahead,incidence_period)
{
  assertthat::assert_that(incidence_period == "epiweek",
                          msg = "Some prediction card is not for epiweek.")
  assertthat::assert_that(response %in% c("deaths_incidence_num","confirmed_incidence_num"),
                          msg = "Some prediction card has wrong response.")
  out = switch(response,
         deaths_incidence_num = paste0(ahead," wk ahead inc death"),
         confirmed_incidence_num = paste0(ahead," wk ahead inc case"))
  out
}
