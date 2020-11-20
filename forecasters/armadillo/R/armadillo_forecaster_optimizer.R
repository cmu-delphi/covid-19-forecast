#' Fit Larry and Valerie's mobility model
#'
#' @param L length of observed response series.
#' @param At normalized parameter.
#' @param beta mobility function parameter.
#' @param alpha mobility function parameter.
#' @param mu mobility function parameter.
#' @param sigma mobility function parameter.
#' @param M mobility series.
#' @param DC death curve.
#' @param before_pan if TRUE, the first estimated response must be 0.
#'
#' @return estmiated response series.
#'
#'
#' @examples
Mean.fun <- function(L, At, beta, alpha, mu, sigma, M, DC, before_pan) {
  Beta <- beta + alpha * pnorm(M[1:L], mu, sigma)
  BetaSum <- cumsum(c(0, Beta))
  BetaSum <- BetaSum[-(L + 1)]
  if (before_pan) {
    out <- rep(0, L)
  } else {
    out <- rep(1, L)
  }
  DC0 <- DC
  for (d in 1:L) {
    DC0 <- c(0, DC0[-L])
    out <- out + DC0 * exp(BetaSum[d])
  }
  return(At * out)
}


#' Hellinger loss function
#'
#' @param y
#' @param yhat
#'
#' @return
#'
#'
#' @examples
Loss <- function(y, yhat) {
  return(mean((sqrt(y) - sqrt(yhat))^2))
}


#' Optimizer using simulated annealing
#'
#' @param param initial value of arguments in Mean.fun (At, beta, alpha, mu, sigma).
#' @param DC death curve.
#' @param y observed response series.
#' @param M mobility series.
#' @param L length of observed response series.
#' @param before_pan same as argument before_pan in Mean.fun.
#' @param lower a vector of lower bounds (At, beta, alpha, mu, sigma).
#' @param upper a vector of upper bounds (At, beta, alpha, mu, sigma).
#' @param ... control arguments in optim_sa, such as initial temperature, ...
#'
#' @return best parameters found by simulated annealing.
#'
#'
#' @examples
fit_optim <- function(param, DC, y, M, L, before_pan, lower, upper, ...) {
  Loss.wrap <- function(x) {
    yhat <- Mean.fun(L, x[1], x[2], x[3], x[4], x[5], M, DC, before_pan)
    loss <- Loss(y[1:L], yhat[1:L])
    return(loss)
  }
  optim_sa(fun = Loss.wrap, start = param, lower = lower, upper = upper, control = list(...))$par
}
