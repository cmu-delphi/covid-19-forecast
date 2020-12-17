#' Creates additional columns and flags outliers for state-level data
#'
#' @param x a data frame as returned by `evalcast:::download_signal()`
#' @param params a list of parameters, typically `correction_pars`
#'
#' @return
#' @export
#' @importFrom dplyr group_by mutate lead lag
#' @importFrom RcppRoll roll_mean roll_meanr roll_median roll_medianr
#' @importFrom RcppRoll roll_sd roll_sdr
state_corrector <- function(x, params) {
  x = dplyr::group_by(x, location)
  with(params,
       dplyr::mutate(x,
              fmean = RcppRoll::roll_meanr(value, window_size),
              smean = RcppRoll::roll_mean(value, window_size, fill = NA),
              fmedian = RcppRoll::roll_medianr(value, window_size),
              smedian = RcppRoll::roll_median(value, window_size, fill = NA),
              fsd = RcppRoll::roll_sdr(value, window_size),
              ssd = RcppRoll::roll_sd(value, window_size,fill = NA),
              fmad = RcppRoll::roll_medianr(abs(value-fmedian), window_size),
              smad = RcppRoll::roll_median(abs(value-smedian), window_size, fill=NA),
              ftstat = abs(value-fmedian)/fsd, # mad in denominator is wrong scale,
              ststat = abs(value-smedian)/ssd, # basically results in all the data flagged
              flag =
                (abs(value) > size_cut & !is.na(ststat) & ststat > sig_cut) | # best case
                (is.na(ststat) & abs(value) > size_cut & !is.na(ftstat) & ftstat > sig_cut) |
                # use filter if smoother is missing
                (value < -size_cut & !is.na(ststat) & !is.na(ftstat)), # big negative
              #(fmean > 10 & fmean< 20 & value > 2*sig_cut*fmean)
              flag = flag | # these allow smaller values to also be outliers if they are consecutive
                (dplyr::lead(flag) & !is.na(ststat) & ststat > sig_consec) |
                (dplyr::lag(flag) & !is.na(ststat) & ststat > sig_consec) |
                (dplyr::lead(flag) & is.na(ststat) & ftstat > sig_consec) |
                (dplyr::lag(flag) & is.na(ststat) & ftstat > sig_consec),
              excess = value,
              corrected = corrections_multinom_roll(
                value, excess, (flag), # & !flag_big_ny),
                time_value, backfill_lag, expectations = value,
                reweight=function(x) exp_w(x, backfill_lag))
              )
       )
}


#' Creates additional columns and flags outliers for county-level data
#'
#' @param x a data frame as returned by `evalcast:::download_signal()`
#' @param params a list of parameters, typically `correction_pars`
#'
#' @return
#' @export
#'
#' @importFrom dplyr group_by mutate lead lag
#' @importFrom RcppRoll roll_mean roll_meanr roll_median roll_medianr
#' @importFrom RcppRoll roll_sd roll_sdr
county_corrector <- function(x, params){
  x = dplyr::group_by(x, location)
  with(params,
       dplyr::mutate(x,
              fmean = RcppRoll::roll_meanr(value, window_size),
              # smean = roll_mean(value, window_size, fill = NA),
              fmedian = RcppRoll::roll_medianr(value, window_size),
              smedian = RcppRoll::roll_median(value, window_size, fill = NA),
              fsd = RcppRoll::roll_sdr(value, window_size),
              ssd = RcppRoll::roll_sd(value, window_size,fill = NA),
              fmad = RcppRoll::roll_medianr(abs(value-fmedian), window_size,na.rm=TRUE),
              smad = RcppRoll::roll_median(abs(value-smedian), na.rm=TRUE),
              ftstat = abs(value-fmedian)/fsd, # mad in denominator is wrong scale,
              ststat = abs(value-smedian)/ssd, # basically results in all the data flagged
              flag =
                (abs(value) > size_cut & !is.na(ststat) & ststat > sig_cut) | # best case
                (is.na(ststat) & abs(value) > size_cut & !is.na(ftstat) & ftstat > sig_cut) |
                # use filter if smoother is missing
                (value < -size_cut & !is.na(ststat) & !is.na(ftstat)), # big negative
              #(fmean > 10 & fmean< 20 & value > 2*sig_cut*fmean)
              flag = flag | # these allow smaller values to also be outliers if they are consecutive
                (dplyr::lead(flag) & !is.na(ststat) & ststat > sig_consec) |
                (dplyr::lag(flag) & !is.na(ststat) & ststat > sig_consec) |
                (dplyr::lead(flag) & is.na(ststat) & ftstat > sig_consec) |
                (dplyr::lag(flag) & is.na(ststat) & ftstat > sig_consec),
              excess = value,
              corrected = corrections_multinom_roll(
                value, excess, flag, time_value, backfill_lag,
                reweight=function(x) exp_w(x, backfill_lag))
              )
       )
}
