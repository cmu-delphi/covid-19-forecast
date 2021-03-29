# helper function to take positive input lags (in the style of Ryan's
# quantgen_forecaster), convert to a list if necessary, and do some error
# checking.
lag_processor <- function(lags, nsigs){
  assert_that(all(unlist(lags) >= 0), msg = "All lags must be nonnegative.")
  if (!is.list(lags)) {
    lags <- rep(list(lags), nsigs)
  } else {
    assert_that(
      length(lags) == nsigs,
      msg = paste("If `lags` is a list, it must have length equal to the number",
                  "of signals."))
  }
  dt <- lapply(lags, "-")
  return(dt)
}
