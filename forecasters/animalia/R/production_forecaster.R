#' Simple quantile autoregressive forecaster based on `quantgen`
#'
#' A simple quantile autoregressive forecaster based on `quantgen`, to be used
#' with `evalcast`, via [evalcast::get_predictions()]. See the 
#' [quantgen forecast vignette](https://cmu-delphi.github.io/covidcast/modeltoolsR/articles/quantgen-forecast.html)
#' for examples.
#'
#' This is a developing variation of [modeltools::quantgen_forecaster()].
#'
#' @param df_list list of signal values to use for forecasting, of the format
#'   that is returned by [covidcast::covidcast_signals()].
#' @template forecast_date-template
#' @param training_window_size Size of the local training window to use. 
#'   For example, if `n = 14`, and `incidence_period = "day"`, then to make a 
#'   1-day-ahead forecast on December 15, we train on
#'   data from November 1 to November 14.
#' @template incidence_period-template
#' @template ahead-template 
#' @param lags Vector of lag values to use as features in the autoregressive
#'   model. For example, setting `lags = c(0, 7, 14)` means we use the 
#'   current value of each signal (defined by an
#'   element of the `df` list), as well as the values 7 and 14 days ago, as the
#'   features. Recall that the response is defined by the first element of the
#'   `df` list. Note that `lags` can also be a list of vectors of lag
#'   values, this list having the same length as the number of signals,
#'   in order to apply a different set of shifts to each signal.
#'   Default is 0, which means no additional lags (only current values) for each
#'   signal. lags are always specified in days.
#' @param tau Vector of quantile levels for the probabilistic forecast. If not
#'   specified, defaults to the levels required by the COVID Forecast Hub.
#' @param lambda vector of values to use for the regularization parameter
#'   in quantile lasso
#' @param signals_to_normalize Should the values be normalized by population?
#'   This can be a single boolean value or a vector of boolean values having the
#'   same length as the number of elements in the `df` list, which tells us
#'   which signals in `df` should be normalized by population and which
#'   should not. Default is `FALSE`, i.e. no normalization.
#' @param transform,inv_trans Transformation and inverse transformations to use
#'   for the response/features. These are applied to the raw data before any
#'   leads or lags. The former `transform` can be a function or a
#'   list of functions, this list having the same length as the number of elements
#'   in the `df` list, in order to apply the same transformation or a
#'   different transformation to each signal. These transformations will be
#'   applied before fitting the quantile model. The latter argument `inv_trans`
#'   specifies the inverse transformation to use on the response variable
#'   (inverse of `transform` if this is a function, or of `transform[[1]]` if
#'   `transform` is a list), which will be applied post prediction from the
#'   quantile model. Several convenience functions for transformations exist as
#'   part of the `quantgen` package. Default is `NULL` for both `transform` and
#'   `inv_trans`, which means no transformations are applied.
#' @param geo_value_selector Function to decide which geo_values to keep
#'   in `df_list`. Predictions will only be made at these geo_values. The 
#'   function must take in a data frame in list format and return a vector
#'   of geo_values. An example of such a function is the returned object from
#'   a call to `select_geo_top_n()`. Default is `NULL`, meaning that all
#'   geo_values in `df_list` are kept.
#' @param featurize Function to construct custom features before the quantile
#'   model is fit. As input, this function must take a data frame with columns
#'   `geo_value`, `time_value`, then the transformed, lagged signal values. This
#'   function must return a data frame with columns `geo_value`, `time_value`,
#'   then any custom features. This function may add/remove rows if desired.
#' @param sort do we sort the predicted quantiles to avoid quantile crossings?
#' @param nonneg do we force the predicted quantiles to be nonnegative?
#' @param noncross Should noncrossing constraints be applied? These force the
#'   predicted quantiles to be properly ordered across all quantile levels being
#'   considered. The default is `FALSE`. If `TRUE`, then noncrossing constraints
#'   are applied to the estimated quantiles at all points specified by the next
#'   argument.
#' @param noncross_points One of "all", "test", "train" indicating which points
#'   to use for the noncrossing constraints: the default "all" means to use both
#'   training and testing sets combined, while "test" or "train" means to use
#'   just one set, training or testing, respectively.
#' @param cv_type One of "forward" or "random", indicating the type of
#'   cross-validation to perform. If "random", then `nfolds` folds are chosen by
#'   dividing training data points randomly (the default being `nfolds = 5`). If
#'   "forward", the default, then we instead use a "forward-validation" approach
#'   that better reflects the way predictions are made in the current time
#'   series forecasting context. Roughly, this works as follows: the data points
#'   from the first `n - nfolds` time values are used for model training, and
#'   then predictions are made at the earliest possible forecast date after this
#'   training period. We march forward one time point at a time and repeat. In
#'   either case ("random" or "forward"), the loss function used for computing
#'   validation error is quantile regression loss (read the documentation for
#'   `quantgen::cv_quantile_lasso()` for more details); and the final quantile
#'   model is refit on the full training set using the validation-optimal tuning
#'   parameter.
#' @param verbose Should progress be printed out to the console? Default is
#'   `FALSE`.
#' @param save_wide_data,save_trained_models if NULL (the default), nothing
#'   happens. Otherwise these should be directory paths to save off the input
#'   data and/or trained model fits. These will be written as RDS files to the
#'   destination directory with names of the form `quantgen_*_forecast_date.RDS`
#' @param ... Additional arguments. Any parameter accepted by
#'   `quantgen::cv_quantile_lasso()` (for model training) or by
#'   `quantgen:::predict.cv_quantile_genlasso()` (for model prediction) can be
#'   passed here. For example, `nfolds`, for specifying the number of folds used
#'   in cross-validation, or `lambda`, for specifying the tuning parameter
#'   values over which to perform cross-validation (the default allows
#'   `quantgen::cv_quantile_lasso()` to set the lambda sequence itself). Note
#'   that fixing a single tuning parameter value (such as `lambda = 0`)
#'   effectively disables cross-validation and fits a quantile model at the
#'   given tuning parameter value (here unregularized quantile autoregression).
#'
#' @return Data frame with columns `ahead`, `geo_value`, `quantile`, and
#'   `value`. The `quantile` column gives the probabilities associated with
#'   quantile forecasts for that location and ahead. This is the format expected
#'   for use with `[evalcast::get_predictions()]`
#'
#' @importFrom dplyr filter select pull summarize bind_cols bind_rows matches
#' @importFrom dplyr starts_with mutate relocate
#' @importFrom tidyr pivot_longer drop_na
#' @importFrom assertthat assert_that
#' @importFrom rlang .data
#' @importFrom purrr map
#' @export
production_forecaster <- function(df_list,
                                  forecast_date,
                                  training_window_size = 28,
                                  incidence_period = c("epiweek", "day"),
                                  ahead = 1:4,
                                  lags = 0,
                                  tau = evalcast::covidhub_probs(),
                                  lambda = 0,
                                  signals_to_normalize = FALSE,
                                  transform = NULL,
                                  inv_trans = NULL,
                                  geo_value_selector = NULL,
                                  featurize = NULL,
                                  sort = TRUE,
                                  nonneg = TRUE,
                                  noncross = FALSE,
                                  noncross_points = c("all", "test", "train"),
                                  cv_type = c("forward", "random"),
                                  verbose = FALSE,
                                  save_wide_data = NULL,
                                  save_trained_models = NULL,
                                  ...){
  # ---------------------
  # 0. standard error checking, likely common to other fit/predict functions
  # we expect the data to be a list
  if (class(df_list)[1] != "list") df_list <- list(df_list)
  nsigs <- length(df_list)
  incidence_period <- match.arg(incidence_period)
  # make lags a list, perform checks
  dt <- lag_processor(lags, nsigs)
  ahead_in_days <- purrr::map_dbl(
    ahead,  ~evalcast::get_target_ahead(forecast_date, incidence_period, .x))
  dt[[1]] <- c(dt[[1]], ahead_in_days) 
  
  # -------------------------------
  # 1. data transformations, and saving
  
  # keep just the geo_values we want
  if (!is.null(geo_value_selector)) {
    geo_values_to_keep <- geo_value_selector(df_list)
    df_list <- map(df_list,
                   ~ .x %>% filter(geo_value %in% geo_values_to_keep))
  }
  
  # apply any transformations (incl. normalizing by population)
  geo_type <- unlist(lapply(df_list, get_geo_type))
  df_list <- normalize_by_population(df_list, geo_type, signals_to_normalize)
  df_list <- transformer(df_list, transform, inv_trans)
  df_wide <- covidcast::aggregate_signals(df_list, dt = dt, format = "wide")
  
  # featurize
  if (!is.null(featurize)) df_wide <- featurize(df_wide)
  
  # rename response
  response_cols <- stringr::str_detect(names(df_wide), "value\\+[1-9]")
  names(df_wide)[response_cols] <- stringr::str_replace(
    names(df_wide)[response_cols], "value", "response")
  names(df_wide)[response_cols] <- stringr::str_replace(
    names(df_wide)[response_cols], "[0-9]+", as.character(ahead))
  
  if (!is.null(save_wide_data)) 
    saveRDS(df_wide, 
            file = file.path(save_wide_data, 
                             sprintf("quantgen_data_for_%s.RDS", forecast_date)))
  # ----------------------------
  # 2. Identify params for quantgen training and prediction functions
  params <- list(...)
  params$tau <- tau
  params$noncross <- noncross
  params$lambda <- lambda
  params$sort <- sort
  params$nonneg <- nonneg
  cv <- is.null(lambda) || length(lambda) > 1
  if (cv) {
    train_fun <- quantgen::cv_quantile_lasso
    predict_fun <- quantgen:::predict.cv_quantile_genlasso
  } else {
    train_fun <- quantgen::quantile_lasso
    predict_fun <- quantgen:::predict.quantile_genlasso
  }
  train_names <- names(as.list(args(train_fun)))
  predict_names <- names(as.list(args(predict_fun)))
  train_params <- params[names(params) %in% train_names]
  predict_params <- params[names(params) %in% predict_names]
  if (noncross) noncross_points <- match.arg(noncross_points)
  if (cv) cv_type <- match.arg(cv_type)
  
  
  # ----------------------
  # 3. Main loop over ahead values, fit model, make predictions
  result <- list()
  if (!is.null(save_trained_models)) trained_models <- list()
  for (a in ahead) {
    if (verbose) cat(sprintf("%s%i", ifelse(a == ahead[1], "\nahead = ", ", "), a))
    
    mats <- modeltools:::create_train_and_predict_matrices(
      df_wide, a, training_window_size)
    
    if (noncross) {
      if (noncross_points == "all") x0 <- rbind(mats$train_x, mats$predict_x)
      else if (noncross_points == "test") x0 <- mats$predict_x
      else if (noncross_points == "train") x0 <- mats$train_x
      x0 <- stats::na.omit(x0) # Just in case, since quantgen won't check this
      train_params$x0 <- x0
    }
    
    # Define forward-validation folds, if we need to
    if (cv && cv_type == "forward")
      train_params$train_test_inds <- forward_cv_idx(
        df_wide$time_value,
        mats$predict_time_value, # needs to come out of modeltools::create_*
        training_window_size,
        a,
        params$nfolds,
        params$ntrain)
    
    # fit model
    train_params$x <- mats$train_x
    train_params$y <- mats$train_y;
    train_obj <- do.call(train_fun, train_params)
    
    # Make predictions
    predict_params$object <- train_obj
    predict_params$object$inv_trans <- inv_trans # Let quantgen handle this
    predict_params$newx <- mats$predict_x
    predict_mat <- do.call(predict_fun, predict_params)
    
    # Do some wrangling to get it into evalcast "long" format
    colnames(predict_mat) <- tau
    predict_df <- bind_cols(geo_value = mats$predict_geo_values, predict_mat) %>%
      pivot_longer(-.data$geo_value, names_to = "quantile", values_to = "value") %>%
      mutate(ahead = a, quantile = as.numeric(.data$quantile)) %>%
      relocate(ahead)
    
    # if we normalized by population, we have to return predictions to the
    # original scale
    predict_df <- normalize_by_population(predict_df, 
                                          geo_type[1],
                                          signals_to_normalize[1],
                                          invert = TRUE)
    
    # save off the objects
    if (!is.null(save_trained_models)) trained_models[[a]] <- train_obj
    result[[a]] = predict_df
  }
  
  if (verbose) cat("\n")
  if (!is.null(save_trained_models)) {
    trained_models <- Filter(function(x) !is.null(x), trained_models)
    names(trained_models) = paste("ahead", "=", ahead)
    saveRDS(trained_models, 
            file = file.path(save_trained_models, 
                             sprintf("quantgen_models_for_%s.RDS", forecast_date)))
  }
  
  # Collapse predictions into one big data frame, and return
  return(bind_rows(result))
  
}
