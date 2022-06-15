
#' Wrapper for [`stats::rbinom`]
#'
#' @template wrapper-for-test-doubles
#'
#' @param n forwarded to `stats::rbinom`
#' @param size forwarded to `stats::rbinom`
#' @param prob forwarded to `stats::rbinom`
#'
#' @return result of `stats::rbinom`
#'
#' @md
rbinom_wrapper <- function(n, size, prob) {
  stats::rbinom(n, size, prob)
}

#' Wrapper for [`stats::rmultinom`]
#'
#' @template wrapper-for-test-doubles
#'
#' @param n forwarded to `stats::rmultinom`
#' @param size forwarded to `stats::rmultinom`
#' @param prob forwarded to `stats::rmultinom`
#'
#' @return result of `stats::rmultinom`
#'
#' @md
rmultinom_wrapper <- function(n, size, prob) {
  stats::rmultinom(n, size, prob)
}
