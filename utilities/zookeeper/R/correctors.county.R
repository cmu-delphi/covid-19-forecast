#' Default parameters for zyzzyva county corrections
#' #'
#' @param signals_to_correct either "response" or "all"
#' @param window_size size of rolling window
#' @param backfill_lag how far back do we fill the spikes
#' @param excess_cut currently ignored
#' @param size_cut "outliers" are ignored if they are smaller in magnitude
#' @param sig_cut t-statistic cut off for marking outliers
#' @param sig_consec slightly smaller t-statistic if consecutive
#' @param time_value_flag_date no corrections after this date
#' @param multinomial_preprocessor logical, do we run the preporcessor
#' @param corrections_db_path path to database if storing results
#' @param integer_tol small number to handle integer checks
#' @param ... ignored
#'
#' @return A list of paramter values
#' @export
#'
#' @examples
#' default_county_params(window_size=21)
default_county_params <- function(signals_to_correct = "confirmed_incidence_num",
                                  window_size = 14,
                                  backfill_lag = 30,
                                  excess_cut = 0,
                                  size_cut = 20,
                                  sig_cut = 3,
                                  sig_consec = 2.25,
                                  time_value_flag_date = Sys.Date() + 1,
                                  multinomial_preprocessor = TRUE,
                                  corrections_db_path = NULL,
                                  integer_tol = 1e-6,
                                  ...) {
  list(
    signals_to_correct = signals_to_correct,
    window_size = window_size,
    backfill_lag = backfill_lag,
    excess_cut = excess_cut,
    size_cut = size_cut,
    sig_cut = sig_cut,
    sig_consec = sig_consec,
    time_value_flag_date = time_value_flag_date,
    multinomial_preprocessor = multinomial_preprocessor,
    integer_tol = integer_tol
  )
}

#' Corrections function for zyzzyva
#'
#' @param signals_list list of signals as returned from `covidcast_signals()`
#'   uses the list (no aggregation)
#' @param ... named corrections parameters passed to `default_county_params()`
#'
#' @return a list of signals the same length and format as the input with
#'   updated entries in the `value` column
#'
#'   If the parameter `corrections_db_path` gives a path, intermediate output
#'   is written to that sqlite database
#' @export
zyzzyva_county_corrections <- function(signals_list, ...){
  if (class(signals_list)[1] == "covidcast_signal") {
    signals_list <- list(signals_list)
  }
  
  params <- default_county_params(...)
  len_params <- sapply(params, length)
  max_len_params <- max(len_params)
  in_names <- names(signals_list[[1]])
  assertthat::assert_that(all(len_params %in% c(1L, max_len_params)),
                          msg = paste("In apply_corrections: ",
                                      "corrections parameters must be length 1 or the",
                                      "same length as the longest corrections parameter."))
  #Single Signal
  if (params$signals_to_correct == "confirmed_incidence_num" | 
      params$signals_to_correct == "deaths_incidence_num") {
    corrected <- zyzzyva_county_corrections_single_signal(
      signals_list[[1]], params)
    signals_list[[1]] <- corrected %>%
      mutate(value = .data$corrected) %>%
      select(all_of(in_names))
  }
  
  # Multiple Signals
  if (all(params$signals_to_correct == c("deaths_incidence_num","confirmed_incidence_num"))|
      all(params$signals_to_correct == c("confirmed_incidence_num","deaths_incidence_num"))) {
    
    corrected <- list()
    for (i in seq_along(signals_list)) {
      corrected[[i]] <- zyzzyva_county_corrections_single_signal(
        signals_list[[i]], params)
    }
    for (i in seq_along(signals_list)) {
      signals_list[[i]] <- corrected[[i]] %>%
        mutate(value = .data$corrected) %>%
        select(all_of(in_names))
    }
  }
  
  if (!is.null(params$corrections_db_path)) {
    corrected_df <- bind_rows(corrected) %>%
      select(.data$data_source, .data$signal, .data$geo_value, .data$time_value,
             .data$value, .data$corrected, .data$flag)
    # save it to the db
    update_corrections(params$corrections_db_path, "county", corrected_df)
  }
  
  return(signals_list)
}


zyzzyva_county_corrections_single_signal <- function(x, params) {
  x <- x %>% group_by(geo_value) %>% 
    dplyr::mutate(
      fmean = roll_meanr(.data$value, params$window_size),
      fmedian = roll_medianr(.data$value, params$window_size),
      smedian = roll_median(.data$value, params$window_size, fill = NA),
      fsd = roll_sdr(.data$value, params$window_size),
      ssd = roll_sd(.data$value, params$window_size, fill = NA),
      fmad = roll_medianr(abs(.data$value-.data$fmedian), params$window_size,na.rm=TRUE),
      smad = roll_median(abs(.data$value-.data$smedian), na.rm=TRUE),
      ftstat = abs(.data$value - .data$fmedian) / .data$fsd,
      ststat = abs(.data$value - .data$smedian) / .data$ssd,
      excess = .data$value - na_replace(.data$smedian, .data$fmedian),
      excess = floor(.data$excess - params$excess_cut*sign(.data$excess)*na_replace(.data$smad,.data$fmad)),
      flag = 
        (abs(.data$value) > params$size_cut & 
            !is.na(.data$ststat) & 
            .data$ststat > params$sig_cut ) | # best case
        (is.na(.data$ststat) & abs(.data$value) > params$size_cut & 
             !is.na(.data$ftstat) & .data$ftstat > params$sig_cut) | 
          # use filter if smoother is missing
        (.data$value < -params$size_cut & 
             !is.na(.data$ststat) & !is.na(.data$ftstat)), # big negative
      flag = .data$flag | # these allow smaller values to also be outliers if they are consecutive
        (dplyr::lead(.data$flag) & !is.na(.data$ststat) & .data$ststat > params$sig_consec) | 
        (dplyr::lag(.data$flag) & !is.na(.data$ststat) & .data$ststat > params$sig_consec) |
        (dplyr::lead(.data$flag) & is.na(.data$ststat) & .data$ftstat > params$sig_consec) |
        (dplyr::lag(.data$flag) & is.na(.data$ststat) & .data$ftstat > params$sig_consec),
      flag = .data$flag & 
        (.data$time_value < ymd(params$time_value_flag_date) | 
            .data$value < -params$size_cut),
      flag = .data$flag | 
        (.data$time_value == "2020-11-20" & as.numeric(.data$geo_value) %/% 1000 == 22),
        #Louisiana backlog drop https://ldh.la.gov/index.cfm/newsroom/detail/5891
      flag_bad_RI = ((substr(.data$geo_value,start = 1, stop = 2) == "44") &
                         .data$value > 10 &
                         abs(lag(.data$value) < params$integer_tol))
      )
        
  if (unique(x$signal) == "confirmed_incidence_num") {
    x <- x %>% mutate(corrected = corrections_multinom_roll( # fix RI reporting problem
                        .data$value, .data$value, .data$flag_bad_RI, .data$time_value, 7),
                      corrected = corrections_multinom_roll( # for everywhere else
                        .data$corrected, .data$excess, (.data$flag & !.data$flag_bad_RI ),
                        .data$time_value, params$backfill_lag,
                        reweight=function(x) exp_w(x, params$backfill_lag)))
  }
   
  
  if (params$multinomial_preprocessor) {
    if (unique(x$signal) == "confirmed_incidence_num") {
      x <- x %>% mutate(corrected = multinomial_roll_sum(.data$corrected))
    }else{
      x <- x %>% mutate(corrected = multinomial_roll_sum(.data$value))
    }
  }
  
  x <- x %>% mutate(corrected = .data$corrected + missing_future(substr(.data$geo_value,start = 1, stop = 2) == "44", 
                                                                 .data$time_value,
                                                                 .data$excess,.data$fmean))
  return(x)
}    




















