
#' Wrapper for [stats::rbinom]
#'
#' @template wrapper-for-test-doubles
#' @md
rbinom_wrapper <- function(n, size, prob) {
  stats::rbinom(n, size, prob)
}

#' Wrapper for [stats::rmultinom]
#'
#' @template wrapper-for-test-doubles
#' @md
rmultinom_wrapper <- function(n, size, prob) {
  stats::rmultinom(n, size, prob)
}
