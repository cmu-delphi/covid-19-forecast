#' Helper function for univariate transformations of COVIDcast signals
#' 
#' Helper function for univariate transformations of COVIDcast signals.
#' 
#' @param df List of signal values to use for forecasting, of the format
#'   that is returned by [covidcast::covidcast_signals()].
#' @param transform,inv_trans Transformation and inverse transformations to use
#'   for the response/features. The former `transform` can be a function or a
#'   list of functions, this list having the same length as the number of elements
#'   in the `df` list, in order to apply the same transformation or a
#'   different transformation to each signal. The latter argument `inv_trans`
#'   specifies the inverse transformation to use on the response variable
#'   (inverse of `transform` if this is a function, or of `transform[[1]]` if
#'   `transform` is a list). Default is `NULL` for both `transform` and
#'   `inv_trans`, which means no transformations are applied.
#' 
#' @return `df` but with the `value` columns appropriately transformed.
transformer <- function(df, transform, inv_trans) {
  if (!is.null(transform)) {
    if (!is.list(transform)) {
      transform = rep(list(transform), length(df))
    }
    else if (length(transform) != length(df)) {
      stop(paste("If `transform` is a list, it must have length equal to the",
                 "number of signals."))
    }
    if (is.null(inv_trans)) {
      stop("If `transform` is specified, then `inv_trans` must be as well.")
    }
    for (i in seq_along(df)) {
      df[[i]] = df[[i]] %>% mutate(value = transform[[i]](.data$value))
    }
  }
  df
}
