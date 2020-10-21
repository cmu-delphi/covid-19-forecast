#' Carnegie Mellon Delphi Lab COVID-19 state death forecaster
#'
#' The production package for the \strong{Carnegie Mellon Delphi Lab} COVID-19 state death forecaster.
#' This forecaster, which is submitted to the CDC as \emph{CMU-TimeSeries} is a basic AR-type time 
#' series model fit using lagged values of case counts and deaths as features. No assumptions 
#' are made regarding reopening or governmental interventions. The model is jointly fit across 
#' all 50 US states, after some time-alignment is performed as a preprocessing step to facilitate 
#' comparison between states. Heavier weight is placed on more recent training data, to account 
#' for nonstationarity in the underlying process. A lasso penalty is added to induce variable 
#' selection and prevent overfitting. Quantiles are computed using a residual (Gaussian) 
#' bootstrap, separately for each location.
#'
#' @name aardvark-package
#' @docType package
#' @author Alden Green and Collin A. Politsch \cr \cr
#' \strong{Maintainer}: Collin A. Politsch <ceubanks@andrew.cmu.edu>
#' @import lubridate
#' @import tidyr
#' @import dplyr
#' @import glmnet
#' @import zoo
#' @importFrom stats as.formula dnorm lm model.matrix poly predict qnbinom 
#' @importFrom stats quantile rnorm weighted.mean end offset start weights
#' @keywords package
#' @seealso Relevant links:
#' \itemize{
#' \item{\url{https://delphi.cmu.edu}}
#' \item{\url{https://covidcast.cmu.edu/}}
#' \item{\url{https://viz.covid19forecasthub.org/}}
#' }
NULL