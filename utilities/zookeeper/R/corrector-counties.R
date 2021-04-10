#' Default parameters for  county case corrections
#'
#' @param data_source covidcast data_source for corrections
#' @param signal vector of covidcast signals to correct
#' @param geo_type the geo_type we're forecasting
#' @param window_size size of rolling window
#' @param backfill_lag how far back do we fill the spikes
#' @param excess_cut currently ignored
#' @param size_cut "outliers" are ignored if they are smaller in magnitude
#' @param sig_cut t-statistic cut off for marking outliers
#' @param sig_consec slightly smaller t-statistic if consecutive
#' @param time_value_flag_date no corrections after this date
#' @param multinomial_preprocessor logical, do we run the preprocessor
#'
#' @return A list of parameter values
#' @export
#'
#' @examples
#' default_state_params(window_size=21)
default_county_params <- function(
    data_source = "jhu-csse",
    signal = c("confirmed_incidence_num"),
    geo_type = "county",
    window_size = 14,
    backfill_lag = 30,
    excess_cut = 0,
    size_cut = 20,
    sig_cut = 3,
    sig_consec = 2.25,
    time_value_flag_date = Sys.Date() + 1,
    multinomial_preprocessor = TRUE) {

  tibble::tibble(
    data_source = data_source,
    signal = signal,
    geo_type = geo_type,
    window_size = window_size,
    backfill_lag = backfill_lag,
    excess_cut = excess_cut,
    size_cut = size_cut,
    sig_cut = sig_cut,
    sig_consec = sig_consec,
    time_value_flag_date = time_value_flag_date,
    multinomial_preprocessor = multinomial_preprocessor
  )
}


#' Corrector for county forecasts
#'
#' This function produces another function to create corrections. It expects
#' the signals from covidcast to be a list (or a single signal).
#'
#' @param params a tibble with corrections parameters. The number of
#'   rows is the number of signals used by the forecaster. The
#'   tibble is most easily generated with [default_state_params()].
#' @param corrections_db_path path to store results, NULL by default
#' @param dump_locations character vector of locations to ignore
#' @param manual_flags tibble with five columns: `geo_value`, `time_value`,
#'   `data_source`, `signal`, and `max_lag` indicating combinations of
#'   of dates, locations, etc to manually flag and back distribute. `max_lag`
#'   determines how far to backfill. For multiple times at one location,
#'   `time_value` may be a list column.
#'
#' @return A function that takes a list of covidcast signals as the only
#'   argument
#' @export
#'
#' @examples
#' make_county_corrector(default_state_params(window_size=21))
make_county_corrector <- function(
  params = default_county_params(),
  corrections_db_path = NULL,
  dump_locations = NULL,
  manual_flags = NULL) {


  function(df) {
    if (class(df)[1] == "covidcast_signal") {
      # in case there's only one signal
      df <- list(df)
    }
    if (!is.null(dump_locations)) {
      df <- purrr::map(df, ~.x %>%
                         dplyr::filter(! geo_value %in% dump_locations))
    }
    params$to_correct <- TRUE # a key for deciding if we make corrections
    in_names <- names(df[[1]])
    all_signals <- df %>%
      purrr::map_dfr(~.x %>%
                       dplyr::select(.data$data_source, .data$signal) %>%
                       head(1))
    params <- dplyr::left_join(all_signals, params)
    if (all(is.na(params$to_correct))) {
      warning(paste("No requested corrections data_source/signal pairs",
                    "were actually present in the data. No corrections",
                    "were implemented."))
      return(df)
    }
    corrected <- list()
    for (i in seq_along(df)) {
      corrected[[i]] <- county_corrections_single_signal(
        df[[i]], params[i,], manual_flags)
      df[[i]] <- corrected[[i]] %>%
        mutate(value = .data$corrected) %>%
        select(all_of(in_names)) %>%
        ungroup()
      df[[i]] <- covidcast::as.covidcast_signal(
        df[[i]],
        signal = df[[i]]$signal[1],
        geo_type = params$geo_type[1])
    }

    if (!is.null(corrections_db_path)) {
      corrected_df <- bind_rows(corrected) %>%
        select(.data$data_source, .data$signal, .data$geo_value,
               .data$time_value,
               .data$value, .data$corrected, .data$flag, .data$special_flag)
      write_rds(corrected_df, file = corrections_db_path)
    }
    return(df)
  }

}



#' @importFrom lubridate ymd
county_corrections_single_signal <- function(x, params, manual_flags) {
  if (is.na(params$to_correct)) {
    # no corrections for this signal
    x <- x %>% mutate(corrected = .data$value)
    return(x)
  }
  if (x$signal[1] == "confirmed_incidence_num" &&
      attr(x, "metadata")$geo_type == "county") {
    x <- x %>% mutate(value = as.integer(round(.data$value)))
  }
  if (params$geo_type == "county") {
    # remove mega-counties
    x <- x %>% filter(as.numeric(.data$geo_value) %% 1000 > 0)
  }
  # actually perform the corrections
  x <- x %>%
    group_by(.data$geo_value) %>%
    dplyr::mutate(
      fmean = roll_meanr(.data$value, params$window_size, na.rm = TRUE),
      fmedian = roll_medianr(.data$value, params$window_size),
      smedian = roll_median(.data$value, params$window_size, fill = NA),
      fsd = roll_sdr(.data$value, params$window_size),
      ssd = roll_sd(.data$value, params$window_size, fill = NA),
      ftstat = abs(.data$value - .data$fmedian) / .data$fsd,
        # mad in denominator is wrong scale,
        # basically results in all the data flagged
      ststat = abs(.data$value - .data$smedian) / .data$ssd,
      flag =
        (abs(.data$value) > params$size_cut &
           !is.na(.data$ststat) &
           .data$ststat > params$sig_cut ) | # best case
        (is.na(.data$ststat) & abs(.data$value) > params$size_cut &
           !is.na(.data$ftstat) & .data$ftstat > params$sig_cut) |
        # use filter if smoother is missing
        (.data$value < -params$size_cut & !is.na(.data$ststat) &
           !is.na(.data$ftstat)), # big negative
      flag = .data$flag | # allow smaller values to also be outliers if consecutive
        (dplyr::lead(.data$flag) & !is.na(.data$ststat) & .data$ststat > params$sig_consec) |
        (dplyr::lag(.data$flag) & !is.na(.data$ststat) & .data$ststat > params$sig_consec) |
        (dplyr::lead(.data$flag) & is.na(.data$ststat) & .data$ftstat > params$sig_consec) |
        (dplyr::lag(.data$flag) & is.na(.data$ststat) & .data$ftstat > params$sig_consec),
      flag = .data$flag &
        (.data$time_value < ymd(params$time_value_flag_date) | .data$value < -params$size_cut),
      flag = .data$flag |
        #Louisiana backlog drop https://ldh.la.gov/index.cfm/newsroom/detail/5891
        (.data$time_value == "2020-11-20" & as.numeric(.data$geo_value) %/% 1000 == 22),
      state = covidcast::fips_to_abbr(paste0(substr(.data$geo_value,1,2),"000"))
    ) %>%
    relocate(.data$state, .after = .data$geo_value) %>%
    mutate(
      flag_bad_RI = (.data$state == "ri"  & .data$value > 10 & dplyr::lag(.data$value) == 0),
      corrected = .data$value,
      special_flag = FALSE
    )


  ds <- x$data_source[1]
  sig <- x$signal[1]
  if (is.null(manual_flags)) {
    manual_flags = tibble::tibble()
  } else {
    manual_flags <- dplyr::filter(manual_flags,
                                  .data$data_source == ds, .data$signal == sig)
  }
  if (nrow(manual_flags) > 0) {
    x <- make_manual_flags(x, manual_flags)
    x <- make_manual_corrections(x, manual_flags)
  }

  x <- x %>%
    dplyr::mutate(
      corrected = corrections_multinom_roll(
        .data$corrected, .data$corrected, .data$flag_bad_RI, .data$time_value, 7)
      )

   # General corrections
  x <- x %>%
    dplyr::mutate(
           special_flag = .data$special_flag | .data$flag_bad_RI,
      corrected = corrections_multinom_roll(
        .data$corrected, .data$value - .data$fmedian,
        (.data$flag & !.data$special_flag),
        .data$time_value, params$backfill_lag,
        expectations = .data$fmedian,
        reweight = function(x) exp_w(x, params$backfill_lag)),
      fmean = roll_meanr(.data$corrected, params$window_size, na.rm = TRUE),
      corrected = .data$corrected +
        missing_future(TRUE, .data$time_value, .data$value, .data$fmean)
    )
  if (params$multinomial_preprocessor) {
    x <- x %>% mutate(corrected = multinomial_roll_sum(.data$corrected))
  }
  return(x)
}

#' @describeIn make_county_corrector alias to avoid destroying production
#' @export
make_zyzzyva_corrector <- make_county_corrector
