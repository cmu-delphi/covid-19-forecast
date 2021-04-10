
na_replace <- function(a, b) {
  stopifnot(length(a) == length(b) || length(b) == 1)
  a[is.na(a)] <- b[is.na(a)]
  a
}


exp_w <- function(x, std_decay = 30, b0 = 8, a = exp(1) / 2){
  stopifnot(length(x) <= std_decay)
  w <- (1:std_decay) / std_decay
  w <- tail(w, length(x))
  1 / (1 + exp(-w * b0 + a))
}

missing_future <- function(selector, time_value, excess, preds) {
  if (!any(excess > 0)) return(0L)
  local_tail <- (selector & time_value > time_value[max(which(excess > 0))])
  if (!any(local_tail)) return(0L)
  tot <- round(sum(preds[local_tail], na.rm = TRUE))
  if (tot <= 0) return(0L)
  stats::rmultinom(1, tot, as.numeric(local_tail))
}

replace_manual <- function(selector, original, replacement) {
  if (sum(selector) != length(replacement)) {
    warning("replacement length does not equal the number of values being replaced")
    return(0L)
  }
  original[selector] <- replacement
  original
}

corrections_multinom_roll <- function(
  x, excess, flag, time_value, max_lag = Inf, expectations = NULL,
  inc_out_time = TRUE, reweight = function(x) rep(1, length(x))) {

  stopifnot(length(x) == length(excess), length(excess) == length(flag))
  stopifnot(is.logical(flag), max_lag == floor(max_lag), max_lag >= 1)
  if (!is.null(expectations) && length(expectations) == 1) expectations <- NULL
  if (length(expectations) > 1) stopifnot(length(expectations) == length(x))

  locs <- which(flag)
  if (length(locs) == 0) return(x)
  if (is.null(expectations)) expectations <- rep(1, length(x))

  excess[is.na(excess)] <- 0
  bad_excess <- abs(excess %% 1) > 1e-10
  if (any(bad_excess)) {
    excess[bad_excess] <- floor(excess[bad_excess]) +
      rbinom(sum(bad_excess), 1, .5)
  }

  for (ii in locs) {
    if (ii <= max_lag) {
      ii_lag <- 1:(ii - 1 + inc_out_time)
    } else {
      ii_lag <- seq(ii - max_lag + 1, ii - 1 + inc_out_time)
    }

    bin_w <- pmax(expectations[ii_lag] /
                    sum(expectations[ii_lag], na.rm = TRUE), 0)
    bin_w[is.na(bin_w)] <- 0

    if (all(bin_w == 0)) bin_w <- rep(1 / length(ii_lag), times = length(ii_lag))

    #reweight bin_w
    zz <- reweight(bin_w)
    bin_w <- zz*bin_w

    x[ii] <- x[ii] - excess[ii]
    prop <- x[ii_lag] + sign(excess[ii]) * stats::rmultinom(1, abs(excess[ii]), bin_w)
    # possibly deal with negatives here
    x[ii_lag] <- prop
  }
  x
}


#'  Two helper functions
#'  (1) Add manually flagged columns, this function will create columns like
#'  "flag_bad_va", "flag_bad_wa", etc.
#'
#'
#' @importFrom rlang :=
#'
make_manual_flags <- function(df, special_flags) {
  ds <- df$data_source[1]
  sig <- df$signal[1]
  special_flags <- dplyr::filter(special_flags,
                                 .data$data_source == ds, .data$signal == sig)
  if (nrow(special_flags) < 1) return(df)
  for (i in seq_len(nrow(special_flags))) {
    varname <- paste0('flag_bad_', special_flags$geo_value[i])
    df <- dplyr::mutate(df,
                        {{varname}} := (
                          .data$geo_value == special_flags$geo_value[i] &
                            as.Date(.data$time_value) %in% unlist(special_flags$time_value[i])
                        ))
  }
  return(df)
}


#' (2) This function makes corrections on manually flagged columns
#' This function will return the original dataframe with two additional columns:
#' `corrected` and `special_flag`, in which the latter one is used to avoid duplicate corrections
#'
make_manual_corrections <- function(df, special_flags){
  for (i in 1:nrow(special_flags)) {
    varname <- paste0('flag_bad_', special_flags$geo_value[i])
    df <- df %>%
      dplyr::mutate(
        corrected = corrections_multinom_roll(
          .data$corrected, .data$corrected - .data$fmedian, .data[[varname]], .data$time_value,
          as.numeric(special_flags$max_lag[i])),
        special_flag = .data$special_flag | .data[[varname]]
      )
  }
  return(df)
}
