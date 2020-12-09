#' Return the aardvark forecaster
#'
#' @description The \link[evalcast]{evalcast-package} production 
#'     evaluator will first call this function to determine all the forecasters
#'     available for the given parameter specifications. It expects to get back 
#'     a named list of lists of forecasting functions and types. If a forecasting 
#'     function is not available for a given set of parameters, an 
#'     \code{NA} should returned instead of a function. This tells the 
#'     evaluator to ignore that forecaster in that run.
#' @param signals Tibble with columns \code{data_source} and signal that specifies 
#' which variables from the COVIDcast API will be used by forecaster. Each row 
#' of signals represents a separate signal, and first row is taken to be the 
#' response.
#' @param ahead The number of incidence periods ahead to forecast the response.
#' For \code{incidence_period = "epiweek"}, one of 1, 2, 3, 4.
#' @param stata_alpha Stratification proportion parameter
#' @param bandwidth Kernel bandwidth (in days) for the local weighting kernel
#' @return A list with an element named \code{aardvark_forecaster}, 
#'     which is itself a list consisting of the forecaster function and a \code{type} 
#'     string (one of \code{c("standalone","ensemble")}), with \code{type = "ensemble"} now 
#'     deprecated. Unavailable forecasters are marked as 
#'     \code{list(forecaster = NA, type = "standalone")}.
#' @export get_forecasters

get_forecasters <- function(signals, ahead, strata_alpha = 0.5, bandwidth = 7){

  response <- paste(signals$data_source[1], signals$signal[1], sep = "-")
  cases <- paste(signals$data_source[1], "confirmed_incidence_num", sep = "-")

  # Modeling pipeline functions to provide forecaster
  imputer <- make_mean_imputer(k = 7, align = "right")
  stratifier <- make_stratifier_by_n_responses(alpha = strata_alpha)
  
  alignment_variable <- cases
  threshold <- 500
  aligner <- make_days_since_threshold_attained_first_time_aligner(variables = alignment_variable,
                                                                   threshold = threshold, 
                                                                   ahead)
  
  # Build autoregressive case and death features
  features <- tibble(variable_name = c(rep(response, 3), rep(cases, 3)), 
                     type = rep("n", 6),
                     offset = rep(F, 6), 
                     main_effect = rep(T, 6), 
                     impute = rep(T, 6)
                     )
  
  if ( ahead == 1 ){
    features[["lag"]] <- rep(c(1, 7, 14), times = 2)
  }else{
    features[["lag"]] <- rep(c((ahead - 1) * 7, (ahead) * 7, (ahead + 1) * 7), times = 2)
  }
  features <- features %>% select(variable_name, type, lag, offset, main_effect, impute)

  # Modeler
  build_penalty_factor <- function(variable_names){
    penalty_factor <- case_when(
      grepl("location", variable_names) ~ 1, # Penalize location-specific effects
      grepl(":", variable_names)        ~ 0, # Don't penalize interactions
      TRUE                             ~ 0 # Don't penalize intercept
    )
  }
  model_fitter <- make_cv_glmnet(alpha = 1, build_penalty_factor = build_penalty_factor)
  model_predicter <- make_predict_glmnet(lambda_choice = "lambda.min")
  modeler <- list(fitter = model_fitter, predicter = model_predicter)
  bootstrapper <- make_by_location_gaussian_bootstrap_weekly(weighted.mean, bandwidth = 14)

  # The final state death forecaster function to pass to the evalcast evaluator
  my_forecaster <- make_aardvark_forecaster(response = response,
                                            features = features,
                                            aligner = aligner,
                                            bandwidth = bandwidth,
                                            stratifier = stratifier,
                                            imputer = imputer,
                                            modeler = modeler,
                                            bootstrapper = bootstrapper)

  # Return the forecaster in the format expected by evalcast
  return(list(aardvark_forecaster = list(forecaster = my_forecaster, type = "standalone")))
}