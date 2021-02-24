
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


  for (ii in locs) {
    if (ii <= max_lag) {
      ii_lag <- 1:(ii -1 + inc_out_time)
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

