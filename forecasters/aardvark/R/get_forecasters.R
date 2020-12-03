#' Get the aardvark forecaster
#'
#' @description The \link[evalcast]{evalcast-package} production 
#'     evaluator will first call this function to determine all the forecasters
#'     available for the given parameter specifications. It expects to get back 
#'     a named list of lists of forecasting functions and types. If a forecasting 
#'     function is not available for a given set of parameters, an 
#'     \code{NA} should returned instead of a function. This tells the 
#'     evaluator to ignore that forecaster in that run.
#' @param response The response variable (\code{geo_type = "state"}), should be 
#' "jhu-csse_deaths_incidence_num" -- the incident deaths reported by Johns Hopkins).
#' @param incidence_period The incidence period ("epiweek" or "day"). This forecaster
#' currently only supports forecasts at the "epiweek" level.
#' @param ahead The number of incidence periods ahead to forecast the response.
#' For \code{incidence_period = "epiweek"}, one of 1, 2, 3, 4.
#' @param forecast_date The date of the forecast.
#' @param geo_type the geographic type (e.g "state" or "county" or
#'     "national", hrr", or "msa"... but for now only the first two),
#' @return A list with an element named \code{aardvark_forecaster}, 
#'     which is itself a list consisting of the forecaster function and a \code{type} 
#'     string (one of \code{c("standalone","ensemble")}), with \code{type = "ensemble"} now 
#'     deprecated. Unavailable forecasters are marked as 
#'     \code{list(forecaster = NA, type = "standalone")}.
#' @export get_forecasters
#' @examples 
#' # Return the Aardvark state death forecaster function for 1 epiweek ahead
#' 
#' ahead = 1
#' signals <- tibble::tibble(data_source = "usa-facts", signal = c("deaths_incidence_num", "confirmed_incidence_num"),
#' start_day = "2020-03-07")
#' my_forecaster <- aardvark::get_forecasters(response_signal = signals$signal[1],
#'   response_source = signals$data_source[1], 
#'   ahead = ahead,
#'   geo_type = "state",
#'   forecast_date = forecast_date)[[1]]$forecaster

get_forecasters <- function(response_source = "jhu-csse", response_signal = "deaths_incidence_num",
                            incidence_period = c("epiweek", "day"), 
                            geo_type = c("state", "county", "national", "hrr", "msa"),
                            ahead, forecast_date, strata_alpha = 0.5){
  
  data_start_date <- lubridate::ymd("2020-03-07")
  incidence_period <- match.arg(incidence_period)
  geo_type <- match.arg(geo_type)
  response <- paste(response_source, response_signal, sep="-")
  cases <- paste(response_source, "confirmed_incidence_num", sep="-")
  stopifnot(geo_type %in% c("state", "county", "national"))
  
  ## Return NULL forecaster until these functionalities are added
  if ( incidence_period != "epiweek" | geo_type %in% c("national", "hrr", "msa") ){
    return(list(aardvark_forecaster = list(forecaster = NA, type = "standalone")))
  }
  
  # Hyperparameters
  bandwidth <- 7
  degree <- 0

  # Modeling pipeline functions to provide forecaster
  stratifier <- make_stratifier_by_n_responses(alpha = strata_alpha)
  imputer <- make_mean_imputer(k = 7, align = "right")
  
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
                     impute = rep(T, 6))
  if ( ahead == 1 ){
    features[["lag"]] <- rep(c(1, 7, 14), times = 2)
  }else{
    features[["lag"]] <- rep(c((ahead - 1) * 7, (ahead) * 7, (ahead + 1) * 7), times = 2)
  }
  features <- features %>% select(variable_name, type, lag, offset, main_effect, impute)

  # Modeler
  build_penalty_factor <- function(variable_names){
    penalty_factor <- case_when(
      grepl("location", variable_names) ~ 1,
      grepl(":", variable_names)        ~ 0,
      TRUE                             ~ 0
    )
  }
  model_fitter <- make_cv_glmnet(alpha = 1, build_penalty_factor = build_penalty_factor)
  model_predicter <- make_predict_glmnet(lambda_choice = "lambda.min")
  modeler <- list(fitter = model_fitter, predicter = model_predicter)
  bootstrapper <- make_by_location_gaussian_bootstrap_weekly(weighted.mean, bandwidth = 14)

  # The final state death forecaster function to pass to the evalcast evaluator
  my_forecaster <- make_aardvark_forecaster(ahead = ahead,
                                            response = response,
                                            features = features,
                                            aligner = aligner,
                                            bandwidth = bandwidth,
                                            degree =  degree,
                                            stratifier = stratifier,
                                            imputer = imputer,
                                            modeler = modeler,
                                            bootstrapper = bootstrapper,
                                            geo_type = geo_type)

  ## Return the forecaster in the format expected by evalcast
  return(list(aardvark_forecaster = list(forecaster = my_forecaster, type = "standalone")))
}