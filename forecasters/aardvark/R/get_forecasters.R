#' Return the aardvark forecaster
#'
#' @description The \link[evalcast]{evalcast-package} production 
#'     evaluator will first call this function to determine all the forecasters
#'     available for the given parameter specifications. It expects to get back 
#'     a named list of lists of forecaster functions and types. If a forecaster 
#'     function is not available for a given set of parameters, an 
#'     \code{NA} should returned instead of a function. This tells the 
#'     evaluator to ignore that forecaster in that run.
#' @param geo_type String indicating geographical type, one of "county", state", 
#'                 or "nation".
#' @param signals Tibble with columns \code{data_source}, \code{signal}, 
#'     \code{start_day} that specify which variables from the COVIDcast API will 
#'     be used by forecaster. Each row of signals represents a separate signal, 
#'     and first row is taken to be the response.
#' @param ahead The number of incidence periods ahead to forecast the response.
#'     For \code{incidence_period = "epiweek"}, one of 1, 2, 3, 4.
#' @param kern The type of kernel to use for smoothing the signals. Right now, 
#'     just "tophat", i.e. boxcar
#' @param strata_alpha Stratification proportion parameter
#' @param bandwidth Kernel bandwidth (in days) for the local weighting kernel
#' @return A list with an element named \code{aardvark_forecaster}, 
#'     which is itself a list consisting of the forecaster function and a \code{type} 
#'     string (one of \code{c("standalone","ensemble")}), with \code{type = "ensemble"} 
#'     now deprecated. Unavailable forecasters are marked as 
#'     \code{list(forecaster = NA, type = "standalone")}.
#' @export get_forecasters
#' @examples 
#'     signals <- dplyr::tibble(data_source = "jhu-csse",
#'     signal = c("deaths_incidence_num", "confirmed_incidence_num"), start_day = "2020-03-07")
#'     ahead <- 1
#'     aardvark_forecaster <- aardvark::get_forecasters(signals = signals, ahead = ahead)[[1]]$forecaster

get_forecasters <- function(geo_type = c("state","county","nation"), signals, ahead, 
                            kern = "tophat", strata_alpha = 0.5, bandwidth = 7){
  
  geo_type <- match.arg(geo_type)
  response <- paste(signals$data_source[1], signals$signal[1], sep = "-")
  cases <- paste(signals$data_source[1], "confirmed_incidence_num", sep = "-")
  
  smoother <- make_kernel_smoother(h = 7, kern = kern)
  stratifier <- make_stratifier_by_n_responses(alpha = strata_alpha)
  aligner <- make_time_aligner(alignment_variable = cases, threshold = 500, ahead = ahead)
  
  model_fitter <- make_cv_glmnet(alpha = 1)
  model_predicter <- make_predict_glmnet(lambda_choice = "lambda.min")
  modeler <- list(fitter = model_fitter, predicter = model_predicter)
  bootstrapper <- make_by_location_gaussian_bootstrap_weekly(weighted.mean, bandwidth = 14)
  
  features <- tibble(variable_name = c(rep(response, 3), rep(cases, 3)))
  if ( ahead == 1 ){
    features[["lag"]] <- rep(c(1, 7, 14), times = 2)
  }else{
    features[["lag"]] <- rep(c((ahead - 1) * 7, (ahead) * 7, (ahead + 1) * 7), times = 2)
  }

  aardvark_forecaster <- make_aardvark_forecaster(geo_type = geo_type,
                                                  response = response,
                                                  features = features,
                                                  smoother = smoother,
                                                  aligner = aligner,
                                                  stratifier = stratifier,
                                                  modeler = modeler,
                                                  bandwidth = bandwidth,
                                                  bootstrapper = bootstrapper)

  return(list(aardvark_forecaster = list(forecaster = aardvark_forecaster, type = "standalone")))
}