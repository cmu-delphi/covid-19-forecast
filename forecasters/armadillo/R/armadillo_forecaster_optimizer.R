#' Fit Larry and Valerie's mobility model
#' @description This function fits Larry and Valerie's mobility model. Given a mobility variable series, the estimated death incidence series is returned.
#'
#' @details  For a single state, let \eqn{Y_t} be the number of deaths at time \eqn{t}. The model can be described as
#' \deqn{E[Y_t|M = m] = A\sum_{s=1}^{t}exp{s\beta + \alpha G_s}f(t-s)}
#' for \eqn{t > 1}, where \eqn{M} denotes mobility, \eqn{m = m_1,...,m_t} is the mobility in the past. \eqn{f(t-s)} is the death delay curve, i.e. the probability of someone dying at time \eqn{t} if infected at time \eqn{s}. The probit function is used for \eqn{G_s}.
#' \deqn{G_s = \sum_{u=1}^{s-1}\Phi((m_u - \mu)/\sigma)}
#' If before_pan is TRUE, \eqn{E[Y_1|M = m] = 0}, otherwise \eqn{E[Y_1|M = m] = A}.
#'
#' @param L length of observed mobility series.
#' @param A normalized parameter. See in the details.
#' @param beta mobility function parameter. See in the details.
#' @param alpha mobility function parameter. See in the details.
#' @param mu mobility function parameter. See in the details.
#' @param sigma mobility function parameter. See in the details.
#' @param M mobility series. See in the details.
#' @param DC death delay curve.
#' @param before_pan if TRUE, the first estimated response must be 0. See in the details.
#'
#' @return Estimated response series.
#' @importFrom stats pnorm
#'
#'
mobility_model <- function(L, A, beta, alpha, mu, sigma, M, DC, before_pan) {
  Beta <- beta + alpha * stats::pnorm(M[1:L], mu, sigma)
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
  A * out
}


#' Hellinger loss function
#'
#' @param y observed response
#' @param yhat fitted response
#'
#' @return Hellinger loss
#'
#'
hellinger_loss <- function(y, yhat) {
  mean((sqrt(y) - sqrt(yhat))^2)
}


#' Optimizer using simulated annealing
#'
#' @param param initial value of arguments in mobility_model (A, beta, alpha, mu, sigma).
#' @param DC death delay curve.
#' @param y observed response series.
#' @param M mobility series.
#' @param L length of observed response series.
#' @param before_pan same as argument before_pan in mobility_model.
#' @param lower a vector of lower bounds (A, beta, alpha, mu, sigma).
#' @param upper a vector of upper bounds (A, beta, alpha, mu, sigma).
#' @param ... control arguments in optim_sa, such as initial temperature, temperature reduction in outer loop, ...
#'
#' @return Best parameters found by simulated annealing.
#' @importFrom optimization optim_sa
#'
fit_optim <- function(param, DC, y, M, L, before_pan, lower, upper, ...) {
  Loss.wrap <- function(x) {
    yhat <- mobility_model(L, x[1], x[2], x[3], x[4], x[5], M, DC, before_pan)
    hellinger_loss(y[1:L], yhat[1:L])
  }
  optimization::optim_sa(fun = Loss.wrap, start = param, lower = lower, upper = upper, control = list(...))$par
}
