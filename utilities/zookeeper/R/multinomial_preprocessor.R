## Used by aardvark, taken from evalforecast,

#' Preprocesses the response values in a data frame.
#'
#' Replaces zeroes (which we suspect are due to lagged reporting) by samples from a multinomial.
#'
#' @param dat a data frame with columns reference_date, location, variable_name, and value
#'       (and possibly some other columns we will ignore.)
#' @param response the name of the response variable.
#' @param max_lag maximum amount counts can be moved. Default to `Inf`
#'
#' @return a data frame of the same format as `dat`
#'

multinomial_preprocessor <- function(dat, response, max_lag = Inf){
  # Preprocesses the response values in a data frame.
  # Replaces zeroes (which we suspect are due to lagged reporting) by samples from a multinomial.
  # Input:
  #  dat: a data frame with columns reference_date, location, variable_name, and value
  #       (and possibly some other columns we will ignore.)
  #  response: the name of the response variable.
  # max_lag: maximum amount counts can be moved.

  # Put data in the right order.
  dat <- dplyr::arrange(dat, .data$reference_date)

  # Impute at each location.
  locs <- unique(dat$location)
  for(ii in 1:length(locs))
  {
    indices <- which(dat$location == locs[ii] & dat$variable_name == response)
    df_use <- dat[indices,]

    # Guard against misuse of this function.
    if( any( duplicated(df_use %>% dplyr::select(reference_date)) ) )
      stop("Multiple response values for the same reference_date. Did you select an issue_date for this reference_date yet?")
    x <- dplyr::pull(df_use,value)
    dat[indices,"value"] <- multinomial_roll_sum(x, max_lag = max_lag)
  }
  return(dat)
}


multinomial_roll_sum <-  function(x, max_lag = Inf){
  # Preprocesses x by replacing NAs by zeros, then eliminating zeros/negatives and corresponding summed counts.
  # E.g. takes x = (0,NA,-10,0,100) and replaces it by x* ~ Multinomial(90, [1/5,1/5,1/5,1/5,1/5])
  # Input: x, a numeric vector, consisting of integers (possibly negative) and NAs.

  # Replace NAs by zeros.
  x <- tidyr::replace_na(x,0)

  # x should be an integer
  if(!all(x == floor(x)))
  {
    warning("Variable is non-integer; to do multinomial trick, replacing it by its floor.")
    x <- floor(x)
  }


  # Find break-points
  breaks <- which(x > 0) + 1
  x_by_break <- unname(split(x, cumsum(seq_along(x) %in% breaks)))

  # Sample from multinomial
  unlist(
    x_by_break %>%
      purrr::map(generate_multinomial, max_lag = max_lag))
}

generate_multinomial <- function(y, max_lag) {
  if (length(y) <= max_lag)
    stats::rmultinom(1,size = max(sum(y),0),prob = rep(1,length(y)))
  else {
    ii_lag <- seq(length(y) - max_lag + 1, length(y))
    c(y[-ii_lag], rmultinom(1,
                            size = max(sum(y[ii_lag]), 0),
                            prob = rep(1, length(ii_lag))))
  }
}
