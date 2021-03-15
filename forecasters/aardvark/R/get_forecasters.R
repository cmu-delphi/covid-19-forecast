#' Return the desired Delphi Lab pandemic forecaster
#'
#' @description The \link[evalcast]{evalcast-package} production evaluator 
#'     will first call this function to determine all the forecasters available
#'     for the given parameter specifications. If a forecaster function 
#'     is not available for a given set of parameters, an \code{NA} should 
#'     returned instead of a function. This tells the evaluator to ignore that
#'     forecaster in that run.
#' @param geo_type String indicating geographical type, such as "county", "state",
#'                 or nation. See the 
#' \href{https://cmu-delphi.github.io/delphi-epidata/api/covidcast_geography.html}{COVIDcast Geographic Coding documentation}
#'                 for available options.
#' @param signals Tibble with columns \code{data_source}, \code{signal}, 
#'     \code{start_day} that specify which variables from the COVIDcast API will 
#'     be used by forecaster. Each row of signals represents a separate signal, 
#'     and first row is taken to be the response.
#' @param ahead The number of incidence periods ahead to forecast the response.
#'     For \code{incidence_period = "epiweek"}, one of 1, 2, 3, 4.
#' @return The forecaster function. Unavailable forecasters return \code{NA}.
#' @export get_forecasters
#' @examples 
#'     state_signals <- dplyr::tibble(data_source = "jhu-csse",
#'                                    signal = c("deaths_incidence_num", "confirmed_incidence_num"), 
#'                                    start_day = "2020-03-07")
#'     ahead <- 1
#'     state_forecaster <- aardvark::get_forecasters(geo_type = "state", 
#'                                                   signals = state_signals, 
#'                                                   ahead = ahead)
#'    \dontrun{
#'    preds <- get_predictions(forecaster = state_forecaster,
#'                             name_of_forecaster = "aardvark",
#'                             signals = state_signals,
#'                             forecast_dates = "2021-02-22",
#'                             incidence_period = "epiweek",
#'                             ahead = ahead,
#'                             geo_type = geo_type,
#'                             apply_corrections = corrections,
#'                             signal_aggregation = "list",
#'                             as_of_override = function(forecast_date){return(as.Date("2021-02-22") + 7)}
#'                             )
#'    }

get_forecasters <- function(geo_type = "state", signals, ahead){
  
  if ( !(geo_type %in% c("county", "state", "nation")) ){
    aardvark_forecaster <- NA
  }

  response <- paste(signals$data_source[1], signals$signal[1], sep = "_")
  cases <- paste(signals$data_source[1], "confirmed_incidence_num", sep = "_")
  
  if ( geo_type == "nation" ){
    
    features <- tibble(variable_name = rep(response, 3))
    if ( ahead == 1 ){
      features[["lag"]] <- c(1, 7, 14)
    }else{
      features[["lag"]] <- c((ahead - 1) * 7, (ahead) * 7, (ahead + 1) * 7)
    }
    
    aligner <- make_time_aligner(alignment_variable = cases, ahead = ahead, threshold = 0)
    model_fitter <- make_fv_glmnet_by_geo_value(n_validation = 35)
    model_predicter <- make_predict_glmnet_by_geo_value()
  }
  
  if ( geo_type == "state" ){
    
    features <- tibble(variable_name = c(rep(response, 3), rep(cases, 3)))
    if ( ahead == 1 ){
      features[["lag"]] <- rep(c(1, 7, 14), times = 2)
    }else{
      features[["lag"]] <- rep(c((ahead - 1) * 7, (ahead) * 7, (ahead + 1) * 7), times = 2)
    }
    
    aligner <- make_time_aligner(alignment_variable = cases, ahead = ahead, threshold = 5000)
    model_fitter <- make_cv_glmnet()
    model_predicter <- make_predict_glmnet()
  }
  
  if ( geo_type == "county" ){
    
    features <- tibble(variable_name = rep(response, 3))
    if ( ahead == 1 ){
      features[["lag"]] <- c(1, 7, 14)
    }else{
      features[["lag"]] <- c((ahead - 1) * 7, (ahead) * 7, (ahead + 1) * 7)
    }
    
    aligner <- make_time_aligner(alignment_variable = response, ahead = ahead, threshold = 50)
    model_fitter <- make_cv_glmnet()
    model_predicter <- make_predict_glmnet()
  }
  
  modeler <- list(fitter = model_fitter, predicter = model_predicter)
  bootstrapper <- make_gaussian_bootstrap_by_geo_value()
  
  aardvark_forecaster <- make_aardvark_forecaster(response = response,
                                                  features = features,
                                                  aligner = aligner,
                                                  modeler = modeler,
                                                  bootstrapper = bootstrapper,
                                                  geo_type_override = geo_type)

  return(aardvark_forecaster)
}
