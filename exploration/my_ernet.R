#' Simple quantile autoregressive forecaster based on asymmetric least squares
#'
#' A simple quantile autoregressive forecaster based on asymmetric least
#' squares as implemented by the `SALES` package, to be used
#' with `evalcast`, via [evalcast::get_predictions()].
#'
#' @param df Data frame of signal values to use for forecasting, of the format
#'   that is returned by [covidcast::covidcast_signals()].
#' @param forecast_date Date object or string of the form "YYYY-MM-DD",
#'   indicating the date on which forecasts will be made. For example, if
#'   `forecast_date = "2020-05-11"`, `incidence_period = "day"`, and `ahead =
#'   3`, then, forecasts would be made for "2020-05-14".
#' @param signals Tibble with columns `data_source` and `signal` that specifies
#'   which variables are being fetched from the COVIDcast API, and populated in
#'   `df`. Each row of `signals` represents a separate signal, and first row is
#'   taken to be the response. An optional column `start_day` can also be
#'   included. This can be a Date object or string in the form "YYYY-MM-DD",
#'   indicating the earliest date of data needed from that data source.
#'   Importantly, `start_day` can also be a function (represented as a list
#'   column) that takes a forecast date and returns a start date for model
#'   training (again, Date object or string in the form "YYYY-MM-DD"). The
#'   latter is useful when the start date should be computed dynamically from
#'   the forecast date (e.g., when the forecaster only trains on the most recent
#'   4 weeks of data).
#' @param incidence_period One of "day or "epiweek", indicating the period over
#'   which forecasts are being made. Default is "day".
#' @param ahead Vector of ahead values, indicating how many days/epiweeks ahead
#'   to forecast. If `incidence_period = "day"`, then `ahead = 1` means the day
#'   after forecast date. If `incidence_period = "epiweek"` and the forecast
#'   date falls on a Sunday or Monday, then `ahead = 1` means the epiweek that
#'   includes the forecast date; if `forecast_date` falls on a Tuesday through
#'   Saturday, then it means the following epiweek.
#' @param n Size of the local training window (in days/weeks, depending on
#'   `incidence_period`) to use. For example, if `n = 14`, and `incidence_period
#'   = "day"`, then to make a 1-day-ahead forecast on December 15, we train on
#'   data from November 1 to November 14.
#' @param lags Vector of lag values to use as features in the autoregressive
#'   model. For example, when `incidence_period = "day"`, setting `lags = c(0,
#'   7, 14)`means we use the current value of each signal (defined by a row of
#'   the `signals` tibble), as well as the values 7 and 14 days ago, as the
#'   features. Recall that the response is defined by the first row of the
#'   `signals` tibble. Note that `lags` can also be a list of vectors of lag
#'   values, this list having the same length as the number of rows of
#'   `signals`, in order to apply a different set of shifts to each signal.
#'   Default is 0, which means no additional lags (only current values) for each
#'   signal.
#' @param tau Vector of quantile levels for the probabilistic forecast. If not
#'   specified, defaults to the levels required by the COVID Forecast Hub.
#' @param featurize Function to construct custom features before the quantile
#'   model is fit. As input, this function must take a data frame with columns
#'   `geo_value`, `time_value`, then the transformed, lagged signal values. This
#'   function must return a data frame with columns `geo_value`, `time_value`,
#'   then any custom features. The rows of the returned data frame *must not* be
#'   reordered.
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
#' @param ... Additional arguments. Any parameter accepted by
#'   `SALES::cv.ernet()` (for model training) or by
#'   `SALES:::predict.cv.ernet()` (for model prediction) can be
#'   passed here (e.g. `nfolds`, `lambda`). Note
#'   that fixing a single tuning parameter value (such as `lambda = 0`)
#'   effectively disables cross-validation and fits a quantile model at the
#'   given tuning parameter value (here unregularized quantile autoregression).
#'
#' @return Data frame with columns `ahead`, `geo_value`, `quantile`, and
#'   `value`. The `quantile` column gives the probabilities associated with
#'   quantile forecasts for that location and ahead.
#'
#' @importFrom dplyr filter select pull summarize between bind_cols
#' @importFrom tidyr pivot_longer
#' @export
ernet_forecaster = function(df, forecast_date, signals, incidence_period,
                            ahead, geo_type,
                            n = 4 * ifelse(incidence_period == "day", 7, 1),
                            lags = 0, tau = modeltools::covidhub_probs,
                            featurize = NULL,
                            cv_type = c("forward", "random"),
                            verbose = FALSE, ...) {
  # Check lags vector or list
  if (any(unlist(lags) < 0)) stop("All lags must be nonnegative.")
  if (!is.list(lags)) lags = rep(list(lags), nrow(signals))
  else if (length(lags) != nrow(signals)) {
    stop(paste("If `lags` is a list, it must have length equal to the number",
               "of signals."))
  }

  # Define dt by flipping the sign of lags, include dt = +ahead as a response
  # shift, for each ahead value, for convenience later
  dt = lapply(lags, "-")
  dt[[1]] = c(dt[[1]], ahead)

  # Append shifts, and aggregate into wide format
  df_wide = covidcast::aggregate_signals(df, dt = dt, format = "wide")

  # Separate out into feature data frame, featurize if we need to
  # DJM: we are "featurizing" the response too

  if (!is.null(featurize)) df_wide = featurize(df_wide)
  # move after since featurizing can alter the time_value
  df_features = df_wide %>%
    select(geo_value, time_value, tidyselect::matches("^value(\\+0|-)"))

  feature_end_date = df_features %>%
    summarize(max(time_value)) %>% pull()

  # Identify params for training and prediction functions
  params = list(...)

  cv = is.null(params$lambda) || length(params$lambda) > 1
  if (cv) {
    train_fun = SALES::cv.ernet
    predict_fun = SALES:::predict.cv.ernet
  }
  else {
    train_fun = SALES::ernet
    predict_fun = predict #SALES:::predict.ernet
  }

  train_names = names(as.list(args(train_fun)))
  predict_names = names(as.list(args(predict_fun)))
  train_params = params[names(params) %in% train_names]
  predict_params = params[names(params) %in% predict_names]

  # # Check cv_type
  # if (cv) cv_type = match.arg(cv_type)

  # Test objects that remain invariant over ahead values
  test_geo_value = df_features %>%
    filter(time_value == max(time_value)) %>%
    select(geo_value) %>% pull()
  newx = df_features %>%
    filter(time_value == max(time_value)) %>%
    select(-c(geo_value, time_value)) %>% as.matrix()

  # Loop over ahead values, fit model, make predictions
  result = vector(mode = "list", length = length(ahead))
  for (i in 1:length(ahead)) {
    a = ahead[i]
    if (verbose) cat(sprintf("%s%i", ifelse(i == 1, "\nahead = ", ", "), a))

    # Training end date
    response_end_date = df_wide %>%
      select(time_value, tidyselect::starts_with(sprintf("value+%i:", a))) %>%
      tidyr::drop_na() %>%
      summarize(max(time_value)) %>% pull()
    train_end_date = min(feature_end_date, response_end_date)

    # Training x and y
    x = df_features %>%
      filter(between(time_value,
                     train_end_date - n + 1,
                     train_end_date)) %>%
      select(-c(geo_value, time_value)) %>% as.matrix()
    y = df_wide %>%
      filter(between(time_value,
                     train_end_date - n + 1,
                     train_end_date)) %>%
      select(tidyselect::starts_with(sprintf("value+%i:", a))) %>% pull()
    
    if (cv) stop("cv not supported yet")
    # # Define forward-validation folds, if we need to
    # if (cv && cv_type == "forward") {
    #   # Training time values
    #   train_time_value = df_wide %>%
    #     filter(between(time_value,
    #                    train_end_date - n + 1,
    #                    train_end_date)) %>%
    #     select(time_value) %>% pull()
    # 
    #   # Training and test folds
    #   nfolds = ifelse(!is.null(params$nfolds), params$nfolds, 5)
    #   train_test_inds = list(train = vector(mode = "list", length = nfolds),
    #                          test = vector(mode = "list", length = nfolds))
    #   for (k in 1:nfolds) {
    #     train_test_inds$train[[k]] = which(
    #       between(train_time_value,
    #               train_end_date - n + k,
    #               train_end_date - nfolds + k - 1))
    #     train_test_inds$test[[k]] = which(
    #       train_time_value == train_end_date - nfolds + k)
    #   }
    #   train_params$train_test_inds = train_test_inds
    # }

    # Add training x and y to training params list, fit model
    # (remove rows that have NAs in them)
    # ernet cannot accept a vector of quantiles in `tau`, so we have to loop
    # over `tau`
    good_inds = rowSums(is.na(x)) == 0 & !is.na(y)
    x = x[good_inds, ]
    y = y[good_inds]
    train_params$x = x
    train_params$y = y
    
    predict_mat <- matrix(NA, nrow = nrow(newx), ncol = length(tau))
    for (j in seq_along(tau)) {
      train_params$tau <- tau[j]
      train_obj = do.call(train_fun, train_params)
      
      # Add training object and newx to test params list, make predictions
      predict_params$object = train_obj
      predict_params$newx = newx
      predict_mat[, j] = do.call(predict_fun, predict_params)
    }
    saveRDS(predict_mat, "ernet-predict_mat.rds")

    # Do some wrangling to get it into evalcast "long" format
    colnames(predict_mat) = tau
    predict_df = bind_cols(geo_value = test_geo_value,
                           predict_mat) %>%
      pivot_longer(cols = -geo_value,
                   names_to = "quantile",
                   values_to = "value") %>%
      mutate(ahead = a)

    # TODO: allow train_obj to be appended to forecaster's output. This would
    # be nice for post-hoc analysis of the fitted models, etc. Two options for
    # doing so: 1. append as a column to returned df; but this would be a big
    # waste of memory since it'd get repeated for each forecast made from the
    # same fitted model, 2. include it as an attribute of the returned df, but
    # unless we're super careful this would get squashed by downstream uses of
    # rbind(), map(), etc. We could be careful here, but then we'd also have
    # to keep track of what evalcast does
    result[[i]] = predict_df
  }
  if (verbose) cat("\n")

  # Collapse predictions into one big data frame, and return
  return(do.call(rbind, result))
}
