#' Helper function for processing lags for production forecaster
#' 
#' Helper function to take positive input lags (in the style of Ryan's
#' `quantgen_forecaster`), convert to a list if necessary, and do some error
#' checking.
#' 
#' @param lags Vector of lag values to use as features in the autoregressive
#'   model. For example, setting `lags = c(0, 7, 14)` means we use the 
#'   current value of each signal as well as the values 7 and 14 days ago, as the
#'   features. Note that `lags` can also be a list of vectors of lag
#'   values, this list having the same length as the number of signals,
#'   in order to apply a different set of shifts to each signal.
#' @param nsigs Number of signals.
#' 
#' @return A list of length `nsigs`, with each element being the vector of
#' (non-positive) lags associated with each signal. When `nsigs` is 1, 
#' returns the vector of lags for that signal.
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
  if(length(dt)==1)
    dt = unlist(dt)
  return(dt)
}
