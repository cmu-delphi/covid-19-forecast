#' Default parameters for state death corrections
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
#' @param multinomial_preprocessor logical, do we run the preporcessor
#'
#' @return A list of paramter values
#' @export
#'
#' @examples
#' default_state_params(window_size=21)
default_state_params <- function(
  data_source = "jhu-csse",
  signal = c("deaths_incidence_num", "confirmed_incidence_num"),
  geo_type = "state",
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


#' Corrector for state forecasts
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
#' make_aardvark_corrector(default_state_params(window_size=21))
make_aardvark_corrector <- function(
  params = default_state_params(),
  corrections_db_path = NULL,
  dump_locations = c("as","gu","mp","vi")) {
  
  
  aardvark_state_corrections <- function(df) {
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
                       select(.data$data_source, .data$signal) %>%
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
      corrected[[i]] <- aardvark_state_corrections_single_signal(
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
  
  return(aardvark_state_corrections)
}
#' 
# We add an argument "special_flags" that consists of lists for each states to the function "aardvark_state_corrections_single_signal"
# There are four components in each state list: geo_value, signal. time_range, and max_lag. 
#' @example 
# list_WA <- list('wa', 'deaths_incidence_num',ymd(c("2020-12-16", "2020-12-17",
#                                                     "2020-12-23", "2020-12-24",
#                                                    "2020-12-29")), 14) 
#
# list_OH <- list('oh', 'deaths_incidence_num',seq(ymd("2021-02-12"),
#                                                   ymd("2021-02-14"), length.out = 3), 60) 
#
# list_VA <- list('va','deaths_incidence_num',seq(ymd("2021-02-20"),ymd(time_value_flag_date),by=1), 60) 
#
# special_flags <- list(list_WA,list_OH,list_VA)



# Two helper functions
# Add special flagging columns, this function will create columns like "flag_bad_va", "flag_bad_wa", etc. 
 special_flagging <- function(df,special_flags){
  for (i in 1:length(special_flags)){
    varname <- paste0('flag_bad_',special_flags[[i]][1])
    df <- mutate(df,{{varname}} := (.data$geo_value == special_flags[[i]][1] &
                                      .data$signal == special_flags[[i]][2]) &
                   as.Date(.data$time_value) %in% unlist(special_flags[[i]][3]))
  }
  return(df)
}


# Make corrections for special flagging columns 
 special_correction <- function(df,special_flags){
     for (i in 1:length(special_flags)){
         varname <- paste0('flag_bad_',special_flags[[i]][1])
         df <- df %>% mutate(corrected = corrections_multinom_roll(
            .data$value, .data$value, .data[[varname]], .data$time_value, as.numeric(special_flags[[i]][4])))
     }
   return(df$corrected)
}


#' @importFrom lubridate ymd
aardvark_state_corrections_single_signal <- function(x, params, special_flags) {
  if (is.na(params$to_correct)) {
    # no corrections for this signal
    x <- x %>% mutate(corrected = .data$value)
    return(x)
  }
  # actually perform the corrections
  x <- x %>% group_by(.data$geo_value) %>%
    dplyr::mutate(
      fmean = roll_meanr(.data$value, params$window_size),
      smean = roll_mean(.data$value, params$window_size, fill = NA),
      fmedian = roll_medianr(.data$value, params$window_size),
      smedian = roll_median(.data$value, params$window_size, fill = NA),
      fsd = roll_sdr(.data$value, params$window_size),
      ssd = roll_sd(.data$value, params$window_size, fill = NA),
      ftstat = abs(.data$value - .data$fmedian) / .data$fsd,
      # mad in denominator is wrong scale,
      # basically results in all the data flagged
      ststat = abs(.data$value - .data$smedian) / .data$ssd,
      flag =
        # best case, use the smoother, helps with big upticks in noise
        (abs(.data$value) > params$size_cut &
           !is.na(.data$ststat) &
           .data$ststat > params$sig_cut) |
        # use filter if smoother is missing
        (is.na(.data$ststat) & abs(.data$value) > params$size_cut &
           !is.na(.data$ftstat) & .data$ftstat > params$sig_cut) |
        # find big negatives
        (.data$value < -params$size_cut &
           (!is.na(.data$ststat) | !is.na(.data$ftstat))),
      flag = .data$flag | # allow smaller values to be outliers if consecutive
        (lead(.data$flag) & !is.na(.data$ststat) & .data$ststat > params$sig_consec) |
        (lag(.data$flag) & !is.na(.data$ststat) & .data$ststat > params$sig_consec) |
        (lead(.data$flag) & is.na(.data$ststat) & .data$ftstat > params$sig_consec) |
        (lag(.data$flag) & is.na(.data$ststat) & .data$ftstat > params$sig_consec),
      flag = .data$flag & # no corrections after some date
        (.data$time_value < ymd(params$time_value_flag_date) |
           .data$value < -params$size_cut),
      # RI is not included in the special flagging process because it's sort of regular 
      flag_bad_RI = (.data$geo_value == "ri" &
                       .data$value > 0 &
                       abs(lag(.data$value) < 1e-6)))
    if (length(special_flags) == 0){
      x <- x
    }else{
      x <- special_flagging(x, special_flags)
    }
  
  # Correction on Rhode Island 
  x_temp <- x %>% mutate(corrected = corrections_multinom_roll(
    .data$value, .data$value, .data$flag_bad_RI, .data$time_value, 7))
  
  # Correction on states that require special correction
  x_temp$corrected <- special_correction(x_temp,special_flags) 
  x <- x_temp
 
  # General corrections
  # Use index to locate the flags that don't overlap with special flags
  x_special <- x %>% 
    mutate(index = row_number()) %>% 
    pivot_longer(c(min(grep("flag_bad",colnames(x))):max(grep("flag_bad",colnames(x)))),
                 names_to = "flag_name",values_to ="flag_value") %>%
    group_by(index) %>%
    filter(flag & flag_value == FALSE) %>% 
    pivot_wider(names_from = flag_name, values_from = flag_value) %>%
    ungroup() %>%
    dplyr::select(-c(corrected, index)) 
  
  # copy of the dataframe for comparsion 
  x_copy <-  x %>% 
    mutate(index = row_number()) %>%
    dplyr::select(-c(corrected,index)) 
  
  # add column ' flag_loc' for flag indication for corrections_multinom_roll
  x <- x  %>% ungroup() %>%
    mutate(flag_loc = (duplicated(rbind(x_special, unique(x_copy)))[-c(1:(nrow(x_special)))])) %>% 
    group_by(.data$geo_value)  
  
  x <- x %>% mutate(
      corrected = corrections_multinom_roll( # for everywhere else
        .data$corrected, .data$value,
        .data$flag_loc,
        .data$time_value, params$backfill_lag,
        reweight = function(x) exp_w(x, params$backfill_lag)),
      corrected = .data$corrected + # imputes forward if necessary
        missing_future(TRUE, .data$time_value, .data$value, .data$fmean)
    )
  if (params$multinomial_preprocessor) {
    x <- x %>% mutate(corrected = multinomial_roll_sum(.data$corrected))
  }
  return(x)
}
