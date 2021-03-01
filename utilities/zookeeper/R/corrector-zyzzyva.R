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
    data_source = "usa-facts",
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
#'
#' @return A function that takes a list of covidcast signals as the only
#'   argument
#' @export
#'
#' @examples
#' make_zyzzyva_corrector(default_state_params(window_size=21))
make_zyzzyva_corrector <- function(
  params = default_county_params(),
  corrections_db_path = NULL,
  dump_locations = NULL) {


  zyzzyva_county_corrections <- function(df) {
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
      corrected[[i]] <- zyzzyva_county_corrections_single_signal(
        df[[i]], params[i,])
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
               .data$value, .data$corrected, .data$flag)
      write_rds(corrected_df, file = corrections_db_path)
    }
    return(df)
  }

  return(zyzzyva_county_corrections)
}



#' @importFrom lubridate ymd
zyzzyva_county_corrections_single_signal <- function(x, params) {
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
      flag = .data$flag | # these allow smaller values to also be outliers if they are consecutive
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
      corrected = corrections_multinom_roll(
        .data$value, .data$value, .data$flag_bad_RI, .data$time_value, 7),
      corrected = corrections_multinom_roll(
        .data$corrected, .data$value, (.data$flag & !.data$flag_bad_RI),
        .data$time_value, params$backfill_lag,
        reweight = function(x) exp_w(x, params$backfill_lag)),
      corrected = .data$corrected +
        missing_future(TRUE, .data$time_value, .data$value, .data$fmean)
    )
  if (params$multinomial_preprocessor) {
    x <- x %>% mutate(corrected = multinomial_roll_sum(.data$corrected))
  }
  return(x)
}
