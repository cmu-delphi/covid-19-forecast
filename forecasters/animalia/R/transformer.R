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
