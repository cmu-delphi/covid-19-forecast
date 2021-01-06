#' Transform mobility variables from day to week
#'
#' This function transforms mobility variables in day to (mean, minimum or maximum over) week If the length of the vector is not a multiple of incidence_length, the first a few data points are excluded to make the length of the left a multiple.
#'
#' @param x vector of mobility variables in day.
#' @param incidence_length by default, 7. Data in day is transformed to week.
#' @param mob_fun function applied to mobility variables. Now this package only supports "min", "max" and "mean".
#'
#' @return Vector of a function of mobility variables in incidence_length.
#'
#'
mob_trans <- function(x, incidence_length = 7, mob_fun) {
  n <- length(x)
  m <- floor(n / incidence_length)
  x <- x[(n %% incidence_length + 1):n]
  x <- matrix(x, nrow = m, ncol = incidence_length, byrow = TRUE)
  mob_fun <- match.fun(mob_fun)
  apply(x, 1, mob_fun)
}


#' Another way to transform mobility variables from day to week
#'
#' Unlike mob_trans, if the length of the vector is not a multiple of incidence_length, the last a few data points are excluded.
#'
#' @param x vector of mobility variables in day.
#' @param incidence_length by default, 7. Data in day is transformed to week.
#' @param mob_fun function applied to mobility variables. Now this package only supports "min", "max" and "mean".
#'
#' @return Vector of a function of mobility variables in incidence_length.
#'
#'
#'
mob_trans_shift <- function(x, incidence_length = 7, mob_fun) {
  n <- length(x)
  m <- floor(n / incidence_length)
  x <- x[1:(m * incidence_length)]
  x <- matrix(x, nrow = m, ncol = incidence_length, byrow = TRUE)
  mob_fun <- match.fun(mob_fun)
  apply(x, 1, mob_fun)
}

#' Transform death incidence numbers from day to sum over a week
#'
#' A vector of death incidence numbers in day is transformed to sum over the incidence length. By default, sum over a week.
#' If the length of the vector is not a multiple of incidence_length, the first a few data points are excluded to make the length of the left a multiple.
#'
#' @param x vector of death incidence numbers in day.
#' @param incidence_length by default, 7.
#'
#' @return Vector of sum of death incidence numbers over the incidence length.
#'
#'
#'
resp_trans <- function(x, incidence_length = 7) {
  n <- length(x)
  m <- floor(n / incidence_length)
  x <- x[(n %% incidence_length + 1):n]
  x <- matrix(x, nrow = m, ncol = incidence_length, byrow = TRUE)
  apply(x, 1, sum)
}
