state_corrector <- function(x, params) {
  x = dplyr::group_by(x, location)
  with(params,
       dplyr::mutate(x,
              fmean = roll_meanr(value, window_size),
              smean = roll_mean(value, window_size, fill = NA),
              fmedian = roll_medianr(value, window_size),
              smedian = roll_median(value, window_size, fill = NA),
              fsd = roll_sdr(value, window_size),
              ssd = roll_sd(value, window_size,fill = NA),
              fmad = roll_medianr(abs(value-fmedian), window_size),
              smad = roll_median(abs(value-smedian), window_size, fill=NA),
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


county_corrector <- function(x, params){
  x = dplyr::group_by(x, location)
  with(params,
       dplyr::mutate(x,
              fmean = roll_meanr(value, window_size),
              # smean = roll_mean(value, window_size, fill = NA),
              fmedian = roll_medianr(value, window_size),
              smedian = roll_median(value, window_size, fill = NA),
              fsd = roll_sdr(value, window_size),
              ssd = roll_sd(value, window_size,fill = NA),
              fmad = roll_medianr(abs(value-fmedian), window_size,na.rm=TRUE),
              smad = roll_median(abs(value-smedian), na.rm=TRUE),
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
