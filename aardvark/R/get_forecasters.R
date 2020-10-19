#' Get the Aardvark state death production forecaster
#'
#' @description The \link[evalforecast]{evalforecast-package} production 
#'     evaluator will first call this function to determine all the forecasters
#'     available for the given parameter specifications. It expects to get back 
#'     a named list of lists of forecasting functions and types. If a forecasting 
#'     function is not available for a given set of parameters, an 
#'     \code{NA} should returned instead of a function. This tells the 
#'     evaluator to ignore that forecaster in that run.
#'     
#' @param response The response variable (for this forecaster, should be 
#' "jhu-csse_deaths_incidence_num" -- the incident deaths reported by Johns Hopkins 
#' University).
#' @param incidence_period The incidence period. This state death forecaster
#' currently only supports forecasts at the "epiweek" level.
#' @param ahead The number of incidence periods ahead to forecast state deaths.
#' One of 1, 2, 3, 4.
#' @param forecast_date The date of the forecast.
#' @param geo_type the geographic type (e.g "state" or "county" or
#'     "hrr" or "msa"... but for now only the first two),
#' @param n_locations The number of geo locations. Up to 52 for \code{geo_type = "state"}
#' (states + D.C. and Puerto Rico)
#' @return A list with an element named \code{aardvark_state_death_forecaster}, 
#'     which is itself a list consisting of the forecaster function and a \code{type} 
#'     string (one of \code{c("standalone","ensemble")}), with \code{type = "ensemble"} now 
#'     deprecated. Unavailable forecasters are marked as 
#'     \code{list(forecaster = NA, type = "standalone")}.
#' @export get_forecasters
#' @examples 
#' # Return the Aardvark state death forecaster function for 1 epiweek ahead
#' library(aardvark)
#' 
#' ahead = 1
#' my_forecaster <- get_forecasters(response = "jhu-csse_deaths_incidence_num",
#'                                  ahead = ahead)[["aardvark_state_death_forecaster"]][["forecaster"]]
# @importFrom evalforecast multinomial_preprocesser

get_forecasters <- function(response = "jhu-csse_deaths_incidence_num", 
                            incidence_period = c("epiweek"), 
                            ahead, 
                            forecast_date,
                            geo_type = c("state", "county", "hrr", "msa"),
                            n_locations = 52){
  
  incidence_period <- match.arg(incidence_period)
  geo_type <- match.arg(geo_type)
  
  if ( geo_type == "county" ){
    cases <- "usa-facts_confirmed_incidence_num"
  }
  else if ( geo_type == "state" ){
    cases <- "jhu-csse_confirmed_incidence_num"
  }
  
  ## Currently only allowing for state death predictions at the epiweek level
  if ( incidence_period != "epiweek" ){
    return(list(
      aardvark_state_death_forecaster = list(forecaster = NA, type = "standalone")
      ))
  }
  
  # Hyperparameters
  data_start_date <- ymd("2020-03-07")
  bandwidth <- 7
  degree <- 0

  # Modeling pipeline functions to provide forecaster
  stratifier <- make_stratifier_by_n_responses(alpha = 0.5)
  preprocesser <- NULL
  imputer <- make_average_imputer(k = 7, align = "center")
  
  if (geo_type == "county"){
    alignment_variable <- "usa-facts_confirmed_cumulative_num"
    threshold <- 500
  }
  else if (geo_type == "state"){
    alignment_variable <- "jhu-csse_confirmed_cumulative_num"
    threshold <- 500
  }
  aligner <- make_days_since_threshold_attained_first_time_aligner(variables = alignment_variable,
                                                                   threshold = threshold, 
                                                                   ahead)
  
  # Build autoregressive case and death features
  features <- tibble(variable_name = c(rep(response, 3), rep(cases, 3)), 
                     type = rep("n", 6),
                     offset = rep(F, 6), 
                     main_effect = rep(T, 6), 
                     impute = rep(T, 6), 
                     interaction = rep(NA, 6))
  features[["build_functions"]] <- rep(NA, 6)
  if ( ahead == 1 ){
    features[["lag"]] <- rep(c(1, 7, 14), times = 2)
  }else{
    features[["lag"]] <- rep(c((ahead - 1) * 7, (ahead) * 7, (ahead + 1) * 7), times = 2)
  }
  features <- features %>% select(variable_name, 
                                  type, 
                                  lag, 
                                  offset, 
                                  main_effect,
                                  impute, 
                                  interaction, 
                                  build_functions)

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

  # The final state death forecaster function to pass to the evalforecast evaluator
  my_forecaster <- make_aardvark_forecaster(ahead = ahead,
                                            response = response,
                                            features = features,
                                            aligner = aligner,
                                            bandwidth = bandwidth,
                                            degree =  degree,
                                            preprocesser = preprocesser,
                                            stratifier = stratifier,
                                            imputer = imputer,
                                            modeler = modeler,
                                            bootstrapper = bootstrapper)

  ## Return the forecaster in the format expected by evalforecast
  return(list(aardvark_state_death_forecaster = list(forecaster = my_forecaster, type = "standalone")))
}
