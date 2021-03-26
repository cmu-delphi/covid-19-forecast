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