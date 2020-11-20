Mean.fun <- function(L, At, beta, alpha, mu, sigma, M, DC, before_pan) {
  #     L = length of death time series. Integer
  #     DC = death curve. Numeric vector
  #     M = mobility time series. Numeric vector
  #     At = parameter = (# of cases at time 1)/normalizing constant. Numeric
  #     alpha, beta, mu, sig = mobility parameters. Numeric
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

Loss <- function(y, yhat) {
  return(mean((sqrt(y) - sqrt(yhat))^2))
}


fit_optim <- function(param, DC, y, M, L, before_pan, lower, upper, ...) {
  Loss.wrap <- function(x) {
    yhat <- Mean.fun(L, x[1], x[2], x[3], x[4], x[5], M, DC, before_pan)
    loss <- Loss(y[1:L], yhat[1:L])
    return(loss)
  }
  optim_sa(fun = Loss.wrap, start = param, lower = lower, upper = upper, control = list(...))$par
}
