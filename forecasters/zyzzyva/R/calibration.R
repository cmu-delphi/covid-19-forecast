## functions for calibration go here
## all functions should be named: `ca.function_name()`


#' Estimate empirical quantiles from predicted quantiles.
#' 
#' Compute the proportion of cases below each nominal quantile. This
#' determines the proper quantile (after interpolation) to request in
#' order to achieve the target quantiles. We currently fix the median
#' to remain the same.
#'
#' @param target_quantiles array of target quantiles, e.g. the cdc quantiles
#' @param predicted_quantiles matrix of predicted quantiles, where each row
#'    corresponds to a location, and each column a quantiles (same scale as y)
#' @param y the true values on the same scale as predicted_quantiles
#' @importFrom stats approx
ca.get_empirical_quantiles <-
  function(target_quantiles, predicted_quantiles, y) {
    stopifnot(nrow(predicted_quantiles) == length(y))
    
    n_quantiles <- length(target_quantiles)
    empirical_prop <- rep(NA, n_quantiles)
    for (i in 1:n_quantiles) {
      empirical_prop[i] <- mean(y < predicted_quantiles[, i], na.rm = T)
    }
    empirical_quantiles <-
      stats::approx(empirical_prop,
             target_quantiles,
             xout = target_quantiles,
             rule = 2)$y
    empirical_quantiles[(n_quantiles + 1) / 2] <- 0.5
    empirical_quantiles
}