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
default_county_params <- function(signals_to_correct = "response",
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

#' Corrections function for aardvark
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
  assert_that(all(len_params %in% c(1L, max_len_params)),
              msg = paste("In apply_corrections: ",
                          "corrections parameters must be length 1 or the",
                          "same length as the longest corrections parameter."))
  #Single Signal
  if (params$signals_to_correct == "response") {
    corrected <- zyzzyva_county_corrections_single_signal(
      signals_list[[1]], params)
    signals_list[[1]] <- corrected %>%
      mutate(value = .data$corrected) %>%
      select(all_of(in_names))
  }
  
  # Multiple Signals
  if (params$signals_to_correct == "all") {
    corrected <- list()
    for (i in seq_along(signals_list)) {
      corrected[[i]] <- zyzzyva_county_corrections_single_signal(
        signals_list[[i]], params)
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
  if (x == "confirmed_incidence_num") {
    x <- x %>% 
      dplyr::mutate(
        fmean = roll_meanr(.data$value, params$window_size),
        smean = roll_mean(.data$value, params$window_size, fill = NA),
        fmedian = roll_medianr(.data$value, params$window_size),
        smedian = roll_median(.data$value, params$window_size, fill = NA),
        fsd = roll_sdr(.data$value, params$window_size),
        ssd = roll_sd(.data$value, params$window_size, fill = NA),
        ftstat = abs(.data$value - .data$fmedian) / .data$fsd,
        ststat = abs(.data$value - .data$smedian) / .data$ssd,
        flag = 
          (abs(value) > size_cut & !is.na(ststat) & ststat > sig_cut ) | # best case
          (is.na(ststat) & abs(value) > size_cut & !is.na(ftstat) & ftstat > sig_cut) | 
        # use filter if smoother is missing
          (value < -size_cut & !is.na(ststat) & !is.na(ftstat)), # big negative
        flag = flag | # these allow smaller values to also be outliers if they are consecutive
          (dplyr::lead(flag) & !is.na(ststat) & ststat > sig_consec) | 
          (dplyr::lag(flag) & !is.na(ststat) & ststat > sig_consec) |
          (dplyr::lead(flag) & is.na(ststat) & ftstat > sig_consec) |
          (dplyr::lag(flag) & is.na(ststat) & ftstat > sig_consec),
        flag = flag & 
          (time_value < ymd(time_value_flag_date) | value < -size_cut),
        flag = flag | 
          (time_value == "2020-11-20" & as.numeric(geo_value) %/% 1000 == 22),
      #Louisiana backlog drop https://ldh.la.gov/index.cfm/newsroom/detail/5891
        flag_bad_RI = (.data$state == "RI"  & .data$value > 10 & abs(lag(.data$value) < params$integer_tol)),
      
        corrected = corrections_multinom_roll( # fix RI reporting problem
          .data$value, .data$value, .data$flag_bad_RI, .data$time_value, 7),
        corrected = corrections_multinom_roll( # for everywhere else
          .data$corrected, .data$value, (.data$flag & !.data$flag_bad_RI ),
          .data$time_value, params$backfill_lag,
          reweight=function(x) exp_w(x, params$backfill_lag)),
        corrected = .data$corrected + # imputes forward due to weekly releases
          missing_future(.data$geo_value == "ri", .data$time_value, .data$value,
                        .data$fmean)
    )
  }
  

  if (params$multinomial_preprocessor) {
    x <- x %>% mutate(corrected = multinomial_roll_sum(.data$corrected))
  }
  return(x)
}    
    
  
  
  
  









      
      
      
      
      

