#-----------------------------------------------------------------------------#
# This script contains the functions needed to build modelers.
# Each modeler is a list comprised of exactly two elements: a fitter and a predictor.
# Hence, this script contains two types of functions: model fitting functions, and model predicting functions.
# Model fitting functions take in data and produce a fitted model.
# A fitted model is a function which can take in data, and produce a (point) prediction.
# All model fitting functions should take the following arguments as inputs:
#   Y: numeric vector
#   X: numeric matrix, with default X = NULL
#   wts: numeric vector, with default wts = rep(1, length(Y))
#   offset: numeric vector, with default offset = NULL
#   intercept: logical, with default intercept = TRUE
#   locs: character vector, identifying each observation with a location.
#
# and possibly
#   t: numeric vector, identifying each observation with a point in "aligned time".
#                      To be used in forward-validation.
#
# Model predicting functions take in a fitted model, and produce predictions.
# All model predicting functions should take the following arguments as inputs:
#   fit: the fitted model used for prediction
#   X: numeric matrix
#   offset: numeric vector
#
#  and possibly
#
#   locs: character vector, identifying each observation with a location.
# These arguments should not have defaults.
#-----------------------------------------------------------------------------#

## The aardvark forecaster fits models which are local in time and estimates
## location-specific effects using shrinkage

make_aardvark_forecaster <- function(ahead = 1, 
                                     incidence_period = c("epiweek", "day"),
                                     geo_type = c("state", "county", "national", "hrr", "msa"),
                                     backfill_buffer = 5,
                                     response = "jhu-csse_deaths_incidence_num", 
                                     features = NULL, 
                                     bandwidth = 7, 
                                     degree = 0, 
                                     intercept = FALSE,
                                     stratifier, 
                                     preprocesser = NULL, 
                                     imputer = NULL, 
                                     modeler = NULL, 
                                     bootstrapper, B = 1000,
                                     aligner = NULL,
                                     verbose = 1){
  # Inputs:
  # 
  #   bandwidth, degree: numeric, the parameters for a local polynomial fit.
  # 
  #   ahead: numeric, how many periods ahead to predict.
  # 
  #   incidence_period: string, one of "epiweek" or "day"
  # 
  #   backfill_buffer: numeric, never use training responses for which issue_date - reference_date < backfill_buffer
  # 
  #   response: string, the name of the variable you would like to treat as response.
  # 
  #   preprocesser: a function to perform preprocessing of our training data, for instance by doing backfill
  # 
  #   imputer: a function to perform smoothing/imputation of our response, to improve training.
  #            the default NULL means we train on the raw response.
  # 
  #   modeler: a named list of length two, with elements fitter and predictor.
  #     modeler$fitter: a function to perform model fitting. 
  #     modeler$predicter: a function to perform prediction.
  #     
  #   bootstrapper: a function used to resample our residuals, to form a distributional estimate.
  # 
  #   B: number of resamples to take when doing Monte Carlo to form distributional estimate.
  # 
  #   features: a data frame, with columns
  #     variable_name: character, the names of the variables to use as features
  #     type: character, the type of the variable. "n" for numeric, "f" for factor.
  #     lag: numeric, how many periods in the past to use for the feature.
  #     offset: logical, whether or not to treat this feature as an offset.
  #     main_effect: logical, whether or not to include a main (global) effect for this feature.
  #     interaction: character, indicating which variable this should be interacted with. NA implies no interactions.
  #     build_functions: list column, with each entry being either NA or a function. 
  #                      If NA, this feature must already belong to the upstream_df.
  #                      If a function, it must take exactly the form of those functions as given in
  #                      build_feature_functions.R.
  # 
  #   intercept: logical, indicating whether or not we want to add a (location-specific) intercept
  # 
  #   aligner: a function, used to compute aligned time.
  #            It must take exactly the form of those functions given in alignment_functions.R.
  # 
  #   verbose: integer, one of 0, 1, 2, or >2. Progressively more printing during forecasting.

  covidhub_probs <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
  
  local_forecaster_with_shrinkage <- function(df, forecast_date, signals, incidence_period, ahead,
                                              geo_type){
    # Inputs:
    #  df: data frame with columns "data_source", "signal", "location", "time_value", 
    #                              "issue", and "value"
    #
    #  forecast_date: the date on which forecasts will be made
    #    about some period ("epiweek" or "day").  Forecaster must only use data that
    #    would have been issued on or before forecast_date.
    
    # Manipulate evalcast df to the format previously used during evalforecast era
    df$geo_value <- covidcast::state_census$ABBR[match(as.numeric(df$location), covidcast::state_census$STATE)]
    df <- df %>% mutate(variable_name = paste(data_source, signal, sep = "-")) %>%
      covidcast::aggregate_signals(format = "wide")
      
    names(df)[which(substr(names(df),1,5) == "value")] <- "value"
    
    # Preamble.

    stopifnot(geo_type != "national")
    forecast_date <- lubridate::ymd(forecast_date)
    target_period <- get_target_period(forecast_date, incidence_period, 
                                       ahead)
    
    # (0) Check some things.
    stopifnot(c("location", "time_value", "issue") %in% names(df))
    stopifnot(names(features) == c("variable_name",
                                   "type", 
                                   "lag",
                                   "offset",
                                   "main_effect",
                                   "impute", 
                                   "interaction",
                                   "build_functions")
    )
    stopifnot(names(modeler) == c("fitter","predicter"))
    stopifnot(is.function(aligner))
    
    
    # (1) Don't use any data past the last_train_date
    df_train <- df %>%
      filter((is.na(issue) & 
                (time_value <= forecast_date)) | issue <= forecast_date | is.na(time_value))
    
    # (2) Concentrate on the locations we need.
    stopifnot("location_to_be_forecast" %in% unique(df_train %>% pull(variable_name)))
    forecast_locations <- df_train %>% 
      filter(variable_name == "location_to_be_forecast" & value == 1) %>%
      pull(location)
    df_train <- filter(df_train, location %in% forecast_locations)
    

    # (3) Concentrate on the variables we need.
    
    ## (A) Build any additional features
    features_df_list <- list()
    build_feature_functions <- features %>% pull(build_functions)
    for ( ii in 1:nrow(features) ){
      if ( is.function(build_feature_functions[[ii]]) ){
        features_df_list[[ii]] <- build_feature_functions[[ii]](features[["variable_name"]][ii], df_train)
      } else{
        features_df_list[[ii]] <- NULL
      }
    }
    df_train <- bind_rows(df_train, features_df_list)
    
    
    ## (C) Filter down to just the variables we need
    alignment_variables <- environment(aligner)$variables
    df_train <- df_train %>% 
      filter(variable_name %in% c(response, 
                                  features$variable_name,
                                  alignment_variables)) %>%
      distinct()
    
    
    # (4) If we have a choice as to issue date...pick the latest one.
    if ( length(unique(df_train$issue)) > 1 ){
      min_issue_date <- min(df_train$issue, na.rm = TRUE) # NAs are always the earliest one.
      df_train <- df_train %>% 
        mutate(issue = replace_na(issue, min_issue_date - 1)) %>%
        group_by(location, reference_date, location_name, variable_name) %>%
        top_n(1, wt = issue) %>% # NA only chosen if that's all there is
        ungroup %>%
        mutate(issue_date = na_if(issue, min_issue_date - 1)) # go back to NA
    }
    
    
    # (5) Don't use any response data that hasn't solidified
    warning("You may be using wobbly features; although your response has stabilized.")
    df_train <- filter(df_train, (variable_name != response) | 
                                 (issue >= time_value + backfill_buffer) |
                                  is.na(issue)) # treat grandfathered data as solidified
    
    # (6) We now never need to deal with issue date again; let's dispose of it.
    df_train <- df_train %>% select(-issue)
    
    ## Bug above here
    
    # Preprocess.
    
    ## (0) Stratification
    ##     "Pretty" locations are locations where we believe our model will be useful.
    ##     "Ugly" locations are locations where we believe a different model will be useful,
    ##     and currently we just use a placeholder.
    all_locs <- df_train %>% pull(location) %>% unique
    locs_ugly_criterion1 <- df_train %>% 
      filter(variable_name == response) %>% group_by(location) %>% 
      summarize(n_response = sum(value, na.rm = T)) %>%
      filter(n_response <= 5) %>% # Ugly because too few responses to model as a continuous variable.
      pull(location) %>% 
      unique()
    df_align <- aligner(df_train, forecast_date)
    locs_ugly_criterion2 <- setdiff(all_locs,
                                    df_align %>% 
                                      filter(!is.na(align_date)) %>% # Ugly because pandemic time hasn't yet begun for this location.
                                      pull(location) %>% unique()) 
    locs_ugly_criterion3 <- 
      setdiff(all_locs,
              df_train %>%
                filter(variable_name == response) %>%
                filter(!is.na(value)) %>% pull(location) %>% unique()) # Ugly because we don't have the response variable for this location.
    locs_ugly <- unique(c(locs_ugly_criterion1, locs_ugly_criterion2, locs_ugly_criterion3))
    
    df_train_pretty <- df_train %>% filter(!(location %in% locs_ugly))
    df_train_ugly <- df_train %>% filter(location %in% locs_ugly)
    
    
    # Predict.
    
    ## (1) Prepare data frame to hold predictions.
    df_all <- expand_grid(location = unique(df_train$location),
                          probs = covidhub_probs)
    
    ## (2) Fit model and issue predictions for non-ugly locations.
    df_preds_pretty <- local_lasso_daily_forecast(df_train_pretty, 
                                                  response,
                                                  degree, bandwidth,
                                                  forecast_date, incidence_period, ahead,
                                                  preprocesser, imputer, stratifier, aligner,
                                                  modeler,
                                                  bootstrapper, B, covidhub_probs,
                                                  features, intercept, alignment_variable,
                                                  verbose = verbose)
    
    ## (3) Fit model and issue predictions for ugly locations.
    warning("Placeholder for a time-to-event model.")
    df_point_preds_ugly <- df_train_ugly %>% 
      filter(variable_name == response) %>%
      group_by(location) %>%
      summarise(preds = pmax(mean(value, na.rm = T), 0)) %>%
      ungroup()
    df_preds_ugly <- expand_grid(df_point_preds_ugly, probs = covidhub_probs) %>%
      mutate(quantiles = qnbinom(p = probs,mu = preds, size = .25)) %>%
      select(-preds)
    non_zero_locs <- filter(df_point_preds_ugly, preds > 0) %>% pull(location) %>% unique()
    df_preds_ugly <- df_preds_ugly %>%
      mutate(quantiles = if_else(
        probs >= .95 & location %in% non_zero_locs,
        pmax(quantiles, 2),
        quantiles
      ))
    
    ## (4) Combine
    df_preds <- bind_rows(df_preds_ugly, df_preds_pretty)
    
    
    ## (5) Replace NA and negative predictions by 0.
    predictions <- left_join(df_all, df_preds, by = c("location", "probs")) %>%
      mutate(quantiles = pmax(replace_na(quantiles,0), 0))
    
    if ( verbose > 0 ){
      print(forecast_date) 
    }
    
    return(predictions)
  }
}

local_lasso_daily_forecast <- function(df_use, 
                                       response, degree, bandwidth,
                                       forecast_date, incidence_period, ahead,
                                       preprocesser, imputer, stratifier, aligner,
                                       modeler, bootstrapper, B, covidhub_probs,
                                       features, intercept, alignment_variable,
                                       verbose = 1){
  # Produces a distributional forecast (represented by quantiles) on daily time series data,
  # using 
  # -- local regression (glmnet with weights given by Gaussian kernel) with ridge? penalty on interactions
  #     to obtain the fitted/predicted conditional mean for each location. 
  # -- Monte Carlo to obtain a distribution of residuals for each location.
  # Train_forecast_dates are used exclusively to ensure that distributional forecasts are obtained in
  # a reasonable manner.
  # Inputs: all inputs are as described in make_local_forecaster_with_shrinkage.
  
  # Use our bootstrapper information to determine our training forecast dates.
  stopifnot(incidence_period == "epiweek") # Currently, we do not support daily forecasting using this function.
  bootstrap_bandwidth <- environment(bootstrapper)$bandwidth
  if ( bootstrap_bandwidth > 7 ){
    train_forecast_dates <- forecast_date - rev(seq(7, bootstrap_bandwidth, by = 7) + (ahead - 1) * 7)
    forecast_dates <- c(train_forecast_dates, forecast_date)
  } else{
    forecast_dates <- forecast_date
  }
  
  # Forecast at each (training or target) forecast date.
  point_preds_list <- list()
  for ( ii in 1:length(forecast_dates) ){
    # (0) Treat train forecast date as forecast date.
    forecast_date_ii <- forecast_dates[ii]
    df_train_use <- filter(df_use, reference_date <= forecast_date_ii | is.na(reference_date))
    
    # (1) Preprocess
    
    ## (A) Only keep variables for which we've observed a non-zero response.
    response_locs <- unique(df_train_use %>% filter(variable_name == response & value > 0) %>% pull(location))
    
    ## (B) Only keep locations for which aligned time is never NA in the target period
    df_align <- aligner(df_train_use, forecast_date_ii)
    target_dates <-  evalcast::get_target_period(forecast_date_ii, incidence_period, ahead) %$%
      seq(start, end, by = "days")
    pretty_locs <- unique(df_align %>% 
                            filter(reference_date %in% target_dates) %>% # dates in the target period
                            group_by(location) %>%
                            summarise(n_na_align_dates = sum(is.na(align_date))) %>% # number of NA align_dates
                            ungroup %>%
                            filter(n_na_align_dates == 0) %>% 
                            pull(location))
    
    if ( length(response_locs) == 0 | length(pretty_locs) == 0 ){
      warning(paste0("Training forecast date",forecast_date_ii,
                     "has no pretty locations; moving on."))
      point_preds_list[[ii]] <- NULL
      next
    }
    
    df_train_use <- filter(df_train_use,location %in% response_locs & location %in% pretty_locs)
    df_original_response <- df_train_use %>% filter(variable_name == response)
    
    ## (C) Impute.
    if ( !is.null(imputer) ){
      
      # (I) Variables to impute.
      impute_variables <- unique(c(response,
                                   features %>% filter(impute) %>% pull(variable_name)))
      df_impute <- filter(df_train_use, variable_name %in% impute_variables)
      df_no_impute <- filter(df_train_use,!(variable_name %in% impute_variables))
      
      # (II) Make sure all variables are present in the data frame.
      location_df <- distinct(select(df_impute, c(location,location_name)))
      df_empty <- expand_grid(location_df,
                              reference_date = unique(df_impute$reference_date),
                              variable_name = unique(df_impute$variable_name))
      df_impute_all <- left_join(df_empty, df_impute, 
                                 by = c("location","location_name","reference_date","variable_name"))
      
      # (III) Impute NA by 0.
      warning("You are treating NA as 0 in the smoothing step.")
      df_impute_all_no_na <- df_impute_all %>%
        mutate(value = if_else(variable_name %in% impute_variables & is.na(value),
                               replace_na(value,0),
                               value))
      
      # (IV) Smooth out our data.
      df_imputed <- df_impute_all_no_na %>%
        group_by(variable_name) %>%
        rename(date = reference_date) %>% # adopt our old convention
        group_modify(~ if(.y$variable_name %in% impute_variables) imputer(.x) else .x) %>% # impute
        rename(original_value = value,value = imputed_value, reference_date = date) %>%
        ungroup()# go back to the new convention
      
      # (V) Add back in variables which were not supposed to be imputed
      df_train_use <- bind_rows(df_imputed,df_no_impute)
    } else{
      df_train_use <- df_train_use %>% mutate(original_value = value)
    }
    
    # (2) Add lagged variables as additional features.
    #     and add rows for all dates (including training period dates and target period dates)
    #     on which we would like forecasts.
    df_with_lags <- make_data_with_lags(df_train_use,
                                        forecast_date_ii, incidence_period, ahead,
                                        response, features)
    
    # (3) Add columns "strata" and "align_date".
    
    ## (A) Compute strata
    df_strata <- stratifier(df_train_use, response)
    
    ## (C) Augment df_with_lags
    df_with_lags <- left_join(df_with_lags, df_align, by = c("location", "reference_date")) %>%
      left_join(df_strata, by = "location")
    
    # (4) Separate predictions for each strata.
    dat_bad <- filter(df_with_lags, !strata)
    if ( nrow(dat_bad) == 0 ){
      warning("No data in bad strata.")
      df_point_preds_bad <- NULL
    } else{
      df_point_preds_bad <- dat_bad %>% 
        local_lasso_daily_forecast_by_stratum(response, degree, bandwidth,
                                              forecast_date_ii, incidence_period, ahead,
                                              features, intercept, df_align, modeler,
                                              verbose)
    }
    
    dat_good <- filter(df_with_lags, strata)
    if ( nrow(dat_good) == 0 ){
      warnings("No data in good strata.")
      df_point_preds_good <- NULL
    } else{
      df_point_preds_good <- dat_good %>% 
        local_lasso_daily_forecast_by_stratum(response, degree, bandwidth,
                                              forecast_date_ii, incidence_period, ahead,
                                              features, intercept, df_align, modeler,
                                              verbose)
    }
    
    # (6) Prepare output, by joining strata and adding original value of the response
    df_point_preds_ii <- bind_rows(df_point_preds_bad, df_point_preds_good) %>%
      left_join(df_use %>% 
                  filter(variable_name == response) %>%
                  select(location, reference_date, value),
                by = c("location","reference_date")
      ) %>%
      rename(original_value = value)
    
    point_preds_list[[ii]] <- df_point_preds_ii
  }
  df_point_preds <- bind_rows(point_preds_list)
  
  # Monte Carlo estimate of distribution of response
  df_bootstrap_preds <- bootstrapper(B, df_point_preds, 
                                     forecast_date, incidence_period, ahead)
  
  # Put response back on original scale.
  df_bootstrap_preds <- df_bootstrap_preds %>%
    pivot_longer(-c(location, location_name, reference_date), 
                 names_to = "replicate", values_to = "value")
  
  
  # If we need to sum over multiple dates...
  df_bootstrap_preds <- df_bootstrap_preds %>%
    group_by(location, location_name, replicate) %>%
    summarise(value = sum(pmax(value, 0))) %>%
    ungroup()
  
  # Obtain quantiles.
  preds_df <- df_bootstrap_preds %>% 
    group_by(location) %>% 
    group_modify(~ data.frame(probs = covidhub_probs,
                              quantiles = round(quantile(.x$value,covidhub_probs))))
  
  return(preds_df)
}

#' @importFrom graphics plot
#' @importFrom magrittr %$%
#' @importFrom evalcast get_target_period
local_lasso_daily_forecast_by_stratum <- function(df_use, response,
                                                  degree, bandwidth,
                                                  forecast_date, incidence_period, ahead,
                                                  features, intercept,
                                                  df_align, modeler,
                                                  verbose = 1){
  # Returns a data frame with point predictions for each (location,reference_date)
  # pair satisfying location in "good" strata, and 
  #                 reference_date in target_period.
  # Inputs:
  #   df_use: data used to make forecasts. At minimum, should have the columns
  #           location, location_name, reference_date, variable_name, original_value, value,
  #           and align_date.
  
  # (1) Preamble
  response_name <- paste0(response,"_lag_0")
  locations <- distinct(df_use %>% filter(variable_name == response_name) %>% select(location,location_name))
  
  # (2) Pivot wider, for model matrix form.
  YX <- df_use %>%
    select(location, align_date, reference_date, variable_name, value) %>% 
    filter(!is.na(align_date)) %>%
    pivot_wider(names_from = "variable_name", values_from = "value") %>%
    rename(response = response_name,
           date = align_date)
  
  ## Save reference date information, so we know which are target rows.
  YX_reference_dates <- YX %>% pull(reference_date)
  YX <- YX %>% select(-reference_date)
  
  # (3) Assign feature the right type.
  for ( ii in 1:nrow(features) ){
    # (A) Get the right name for the feature (i.e. the name it will have in YX)
    if(is.na(features$lag[ii])){
      feature_name <- features$variable_name[ii]
    } else{
      feature_name <- paste0(features$variable_name[ii],"_lag_",features$lag[ii])
    }
    stopifnot(feature_name %in% names(YX))
    
    # (B) Recast the column type in YX
    stopifnot(features$type %in% c("n","f"))
    if ( features$type[ii] == "n" ){
      YX[[feature_name]] <- as.numeric(YX[[feature_name]])
    } else if ( features$type[ii] == "f" ){
      YX[[feature_name]] <- as.factor(YX[[feature_name]])
    }
  }
  
  # Do some checks.
  nas_by_feature <- colSums(is.na(YX %>% select(-response)))
  if ( any(nas_by_feature > 0) ){
    na_features <- names(YX %>% select(-response))[nas_by_feature > 0]
    stop("The following features: ", paste0(na_features,collapse = " and "),
         " have NA values.")
  }
  
  # (This check makes sure our stratifier has screened out any variables for which
  #  all data has happened on an NA date; recall that align_date might be NA.)
  stopifnot(length(setdiff(locations %>% pull(location),
                           YX %>% pull(location) %>% unique)) == 0)
  
  # (3) If we have specified an offset, extract it.
  offset <- NULL
  if ( !is.null(features) ){
    offset_df <- filter(features,offset)
    feature_df <- filter(features,!offset)
    stopifnot(nrow(offset_df) <= 1)
    if ( nrow(offset_df) > 0 ){
      offset_variable <- paste0(offset_df$variable_name,"_lag_",offset_df$lag)
      if ( offset_variable %in% names(YX) ){
        offset <- YX %>% pull(offset_variable)
        if ( is.null(feature_df) | !(offset_df$variable_name %in% feature_df$variable_name )){
          # Remove the offset variable, unless it is also a feature.
          YX <- YX %>% select(-offset_variable)
        }
      }
    }
  }
  
  # (4) Get predicted values.
  
  ## (A) Get all the dates we would like predicted values for.
  target_dates <- evalcast::get_target_period(forecast_date,incidence_period,ahead) %$%
    seq(start,end,by = "days")
  dates <- unique(
    df_align %>% 
      filter(location %in% locations$location,      
             reference_date %in% target_dates) %>% 
      pull(align_date)
  )
  
  ## (B) One fit per time
  preds <- list()
  for ( ii in 1:length(dates) ){
    # (I) Add time relative to date.
    YX_use <- YX %>% 
      mutate(t = as.numeric(date - dates[ii]))
    
    # (II) Remember which rows we should predict
    forecast_rows <- which(YX_use$date == dates[ii] & # right align date
                             YX_reference_dates %in% target_dates # right reference date
    )
    forecast_locs <- YX_use[forecast_rows,] %>% pull(location)
    forecast_reference_dates <- YX_reference_dates[forecast_rows]
    stopifnot(unique(forecast_locs) == forecast_locs)
    
    # (III) We don't need date any more.
    YX_use <- YX_use %>% select(-date)
    
    # (IV) Add polynomials in t.
    if ( degree > 0 ){
      t_use <- data.frame(poly(YX_use$t, degree = degree))
      names(t_use) <- paste0("t",1:degree)
      YX_use <- bind_cols(YX_use,t_use)
    }
    
    # (V) Compute weights as a function of t
    wts <- dnorm( YX_use$t  / bandwidth )
    
    # (VI) Strip t from YX_use, but hold on to it as a vector
    t <- YX_use %>% pull(t)
    YX_use <- YX_use %>% 
      select(-t)
    
    # (VII) Build training and test sets
    train_indices <- !is.na(YX_use$response) # response is NA only in the target period...
    X_train_test <- model_matrix(YX_use,intercept,features)
    X_train <- X_train_test[train_indices,,drop = F]
    Y_train <- (YX_use %>% pull(response))[train_indices]
    wts_train <- wts[train_indices]
    offset_train <- offset[train_indices]
    X_test <- X_train_test[forecast_rows,,drop = F] # Keep a one row matrix as a matrix.
    offset_test <- offset[forecast_rows]
    
    
    # (VIII) Fit our model
    train_locs <- (YX_use %>% pull(location))[train_indices]
    train_t <- t[train_indices]
    fit <- modeler$fitter(Y = Y_train,
                          X = X_train, 
                          wts = wts_train, 
                          offset = offset_train, 
                          intercept = intercept,
                          locs = train_locs,
                          t = train_t)
    
    # (IX) Predict using our model.
    preds_ii <- data.frame(location = forecast_locs,
                           reference_date = forecast_reference_dates,
                           preds = 
                             modeler$predicter(fit  = fit,
                                               X = X_test,
                                               offset = offset_test,
                                               locs = forecast_locs))
    
    preds[[ii]] <- preds_ii
    # Some prints to let Alden know what the size of training and test sets are.
    if ( verbose > 1 ){
      print(paste0("Training X has total number of rows: ", dim(X_train)[1]))
      print(paste0("Testing X has total number of rows: ", dim(X_test)[1]))
    }
    if ( verbose > 2 ){
      graphics::plot(fit)
    }
  }
  df_preds <- bind_rows(preds)
  
  # (5) Prepare output
  df_empty <- expand_grid(locations,reference_date = target_dates, strata = df_use$strata[1])
  df_final <- left_join(df_empty,df_preds, by = c("location","reference_date"))
  
  # Some prints to let Alden know how far along the forecaster is.
  if (verbose > 0){
    print(paste0("Forecast date: ", forecast_date))
    print(paste0("Strata: ", df_use$strata[1]))
  }
  return(df_final)
}


#' @importFrom evalcast get_target_period
#' @import purrr
make_data_with_lags <- function(df_use, forecast_date, incidence_period, ahead,
                                response, features){
  ## This function assembles all the data we will need for training and predicting.
  ## This means **I guarantee** the output of this function should have 
  ## an entry for each (variable_name, location,date) triple
  ## in either my training or test period. 
  ##
  ## This guarantee should hold for both temporal and non-temporal variables,
  ## which we do by treating non-temporal variables like a time series which only ever has
  ## one value.
  
  # (1) Separate temporal and non-temporal variables
  df_temporal <- filter(df_use, !is.na(reference_date))
  df_non_temporal <- filter(df_use,is.na(reference_date))
  
  # (2) Get all the locations and dates we need.
  locations <- distinct(df_use %>% filter(variable_name == response) %>% select(location,location_name))
  target_period <- get_target_period(forecast_date,incidence_period,ahead)
  target_dates <- seq(target_period$start,target_period$end,by = "days")
  reference_dates <- unique(c(df_temporal %>% pull(reference_date),target_dates))
  
  # (3) Build df for non-temporal variables.
  non_temporal_vars <- intersect(unique(df_non_temporal$variable_name), c(response,features$variable_name))
  if (length(non_temporal_vars) > 0){
    df_non_temporal_all <- expand_grid(locations,
                                       reference_date = reference_dates,
                                       variable_name = non_temporal_vars) %>%
      left_join(df_non_temporal %>% select(-reference_date), 
                by = c("location","location_name","variable_name"))
  } else{
    df_non_temporal_all <- NULL
  }
  
  # (4) Build df for temporal variables.
  
  ## (A) All possible location, date, variable_name triples.
  all_dates <- seq(min(reference_dates),max(reference_dates),by = 1)
  temporal_vars <- intersect(unique(df_temporal$variable_name), c(response,features$variable_name))
  stopifnot(length(temporal_vars) > 0)
  df_temporal_all <- expand_grid(locations,
                                 reference_date = all_dates,
                                 variable_name = temporal_vars) %>%
    left_join(df_temporal,
              by = c("location", "location_name","reference_date","variable_name"))
  
  ## (c) Add lags.
  lags <- setdiff( unique(features$lag), NA)
  if (!is.null(lags)){
    stopifnot(is.numeric(lags))
    
    # Lags for all variables.
    lag_functions <- lags %>% 
      map(function(x) ~ 
            na.locf(lag(., n = x, default = first(.))))
    names(lag_functions) <- paste0("lag_", lags)
    
    df_temporal_all_with_lags <- df_temporal_all %>%
      group_by(location, variable_name) %>%
      arrange(reference_date) %>%
      mutate_at(vars(value),.funs = lag_functions) %>%
      ungroup() %>%
      rename(lag_0 = value) %>%
      pivot_longer(contains("lag_"),
                   names_to = "lag",values_to = "value")
    
    # Tidy up rows, by just selecting the dates we want.
    df_temporal_all_with_lags <- df_temporal_all_with_lags %>%
      filter(reference_date %in% reference_dates)
    
    # Tidy up columns
    df_temporal_all_with_lags <- df_temporal_all_with_lags %>%
      mutate(variable_name = paste0(variable_name,"_",lag)) %>%
      select(-lag)
    
    # Just select the ones we want. 
    feature_names <- paste0(features$variable_name,"_lag_",features$lag)
    response_name <- paste0(response,"_lag_",0)
    df_temporal_with_lags <- df_temporal_all_with_lags %>% 
      filter(variable_name %in% c(feature_names,response_name))
    
  } else{
    df_temporal_with_lags <- df_temporal_all %>%
      mutate(variable_name = paste0(variable_name,"_lag_0"))
  }
  df_with_lags <- bind_rows(df_temporal_with_lags,df_non_temporal_all)
  return(df_with_lags)
}

#' @importFrom Matrix Matrix
model_matrix <- function(dat, intercept = TRUE, features = NULL){
  # A wrapper around model.matrix,
  # allowing us to dynamically build the formula we would like to feed to model matrix.
  X_frame <- dat %>% select(-response)
  
  # TODO: If there is an offset, regress it out from each of the features.
  if ( length(unique(X_frame$location)) > 1 ){
    X_formula <- model_formula(features, intercept)
    X_train <- Matrix::Matrix( model.matrix(X_formula, X_frame), sparse = T)
  } else{
    if ( all(names(X_frame) == "location") ){
      X_train <- matrix(0, ncol = 1, nrow = nrow(X_frame))
      colnames(X_train) <- "zero_col"
    } else{
      X_train <- data.matrix(X_frame %>% select(-location))
    }
    
  }
  return(X_train)
}

model_formula <- function(features, intercept){
  # A function to create a formula with the main effects, interactions, and 
  # intercepts we want.
  # Inputs:
  # -- features, intercept: see above.
  
  # (1) Create the main effect part of the formula
  main_effect_features <- features %>% 
    filter(main_effect) %>%
    mutate(feature_name = paste0(variable_name, "_lag_", lag)) %>%
    mutate(feature_name = if_else(grepl("-", feature_name), 
                                  paste0("`", feature_name, "`"),
                                  feature_name)) %>%
    pull(feature_name)
  main_effect_chr <- paste0(main_effect_features, collapse = " + ")
  if (intercept) {
    main_effect_chr <- paste0(main_effect_chr, "+ location - 1")
  } else{
    main_effect_chr <- paste0(main_effect_chr, "- 1")
  }
  
  # (1) Create the interaction part of the formula
  if ( nrow(features %>% filter(!is.na(interaction))) > 0 ){
    interaction_specs <- features %>%
      filter(!is.na(interaction)) %>%
      select(variable_name, interaction,lag) %>%
      left_join(features %>% 
                  select(variable_name,lag) %>%
                  rename(interaction_lag = lag), by = c("interaction" = "variable_name")) %>%
      filter(  lag == interaction_lag | # Include interactions only between same lagged variables
                 is.na(interaction_lag) | is.na(lag))            # or with non-temporal variables.
    interaction_features <- interaction_specs %>%
      filter(!is.na(interaction)) %>%
      mutate(feature_name = case_when(
        is.na(lag) ~ variable_name,
        TRUE       ~ paste0(variable_name,"_lag_",lag)
      )) %>%
      mutate(interaction_feature_name = case_when(
        interaction == "location" ~ "location",
        is.na(interaction_lag) ~ interaction,
        TRUE                   ~ paste0(interaction, "_lag_", interaction_lag)
      ))  %>%
      mutate(feature_name = if_else(grepl("-", feature_name), 
                                    paste0("`", feature_name, "`"),
                                    feature_name),
             interaction_feature_name = if_else(grepl("-", interaction_feature_name), 
                                                paste0("`", interaction_feature_name, "`"),
                                                interaction_feature_name)) %>%
      mutate(feature_and_interaction_name = paste0(interaction_feature_name,":", feature_name)) %>%
      pull(feature_and_interaction_name)
    interaction_chr <- paste0(interaction_features, collapse = " + ")
  } else{
    interaction_chr <- NULL
  }
  
  # (3) Combine main effects and interactions
  formula_chr <- paste0( c("~ ", main_effect_chr, interaction_chr), collapse = " + ")
  return(as.formula(formula_chr))
}

make_cv_glmnet <- function(alpha = 1, build_penalty_factor, fdev = 0, mnlam = 100, n_folds = 10){
  # Closure, allowing us to pass tuning parameters to cv.glmnet
  # Inputs:
  #   alpha: numeric between 0 and 1
  #   build_penalty_factor: function, taking as input the names of X and producing output which
  #                         can be passed to cv.glmnet as the penalty.factor argument
  #   fdev, mnlam: parameters to be passed to glmnet.control().
  #                See help(glmnet.control) for details.
  #   n_folds: number of folds to use for cross-validation.
  
  stopifnot(is.numeric(alpha) & 0 <= alpha & alpha <= 1)
  stopifnot(is.function(build_penalty_factor))
  cv_glmnet <- function(Y, X, wts, offset, intercept, locs, ...){
    # See top of the script for description re: inputs.
    stopifnot(is.character(locs))
    
    # (1) Penalize interactions, either with locations or otherwise.
    penalty_factor <- build_penalty_factor(colnames(X))
    if ( all(penalty_factor == 0) ){
      # Ad-hoc adjustment --- when nothing is being penalized ---
      # is to penalize everything equally.
      penalty_factor <- rep(1, length(penalty_factor))
    }
    
    # (2) Determine folds for cross validation.
    unq_locs <- unique(locs)
    stopifnot(length(unq_locs) >= n_folds) # Need something to hold out.
    fold_for_each_loc <- rep(1:n_folds, length.out = length(unq_locs))
    names(fold_for_each_loc) <- unq_locs
    fold_id <- sapply(locs, FUN = function(loc){which(names(fold_for_each_loc) == loc)})
    
    # (3) Fit our model.
    glmnet.control(fdev = fdev, mnlam = mnlam)
    cv.glmnet(x = X, y = Y,
              alpha = alpha, weights = wts, offset = offset,
              penalty.factor = penalty_factor, intercept = intercept,
              nfolds = n_folds, foldid = fold_id,
              type.measure = "mse")
  }
}

make_predict_glmnet <- function(lambda_choice){
  # Closure, allowing us to pass parameters to predict.glmnet.
  # Inputs:
  #   lambda_choice: one of "lambda.1se" or "lambda.min", determining how
  #                  we will choose the value of lambda used for prediction.
  predict_glmnet <- function(fit, X, offset,...){
    stopifnot(is.character(lambda_choice))
    preds <- predict(fit, newx = X, newoffset = offset, s = lambda_choice)[,1]
    return(preds)
  }
}