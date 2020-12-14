#' @import covidcast
make_aardvark_forecaster <- function(response = NULL, features = NULL, backfill_buffer = 5, 
                                     bandwidth = 7, degree = 0, stratifier = NULL, modeler = NULL, 
                                     aligner = NULL, bootstrapper, B = 1000){
  
  covidhub_probs <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
  
  local_forecaster_with_shrinkage <- function(df, forecast_date, signals, incidence_period = c("epiweek","day"),
                                              ahead, geo_type){
    
    incidence_period <- match.arg(incidence_period)
    forecast_date <- lubridate::ymd(forecast_date)
    target_period <- get_target_period(forecast_date, incidence_period, ahead)
    alignment_variable <- environment(aligner)$alignment_variable

    df_train <- df %>% bind_rows %>% long_to_wide %>%
      filter(variable_name %in% c(response, features$variable_name, alignment_variable)) %>% distinct %>% 
      filter((variable_name != response) | (issue >= time_value + backfill_buffer) | is.na(issue)) %>%
      select(-issue)
    
    df_all <- expand_grid(location = unique(df_train$location), probs = covidhub_probs)
    df_preds <- local_lasso_daily_forecast(df_train, response, degree, bandwidth, forecast_date, incidence_period,
                                           ahead, stratifier, aligner, modeler, bootstrapper, B, covidhub_probs, 
                                           features, alignment_variable)
    
    predictions <- left_join(df_all, df_preds, by = c("location", "probs")) %>%
      mutate(quantiles = pmax(replace_na(quantiles, 0), 0), ahead = ahead,
             geo_value = covidcast::state_census$ABBR[match(as.numeric(location),covidcast::state_census$STATE)]) %>% 
      select(location, geo_value, ahead, probs, quantiles) %>% mutate(ahead = as.integer(ahead)) %>% arrange(geo_value)
    return(predictions)
  }
}

#' @importFrom magrittr %$%
local_lasso_daily_forecast <- function(df_use, response, degree, bandwidth, forecast_date, incidence_period, ahead,
                                       stratifier, aligner, modeler, bootstrapper, B, covidhub_probs, features, 
                                       alignment_variable){

  bootstrap_bandwidth <- environment(bootstrapper)$bandwidth
  if ( bootstrap_bandwidth > 7 ){
    train_forecast_dates <- forecast_date - rev(seq(7, bootstrap_bandwidth, by = 7) + (ahead - 1) * 7)
    forecast_dates <- c(train_forecast_dates, forecast_date)
  } else{
    forecast_dates <- forecast_date
  }

  point_preds_list <- list()
  for ( itr in 1:length(forecast_dates) ){

    df_train_use <- df_use %>% filter(time_value <= forecast_dates[itr] | is.na(time_value))

    locs1 <- df_train_use %>% filter(variable_name == response & value > 0) %>% pull(location) %>% unique
    df_align <- aligner(df_train_use, forecast_dates[itr])
    target_dates <-  evalcast::get_target_period(forecast_dates[itr], incidence_period, ahead) %$%
      seq(start, end, by = "days")
    locs2 <- df_align %>% filter(time_value %in% target_dates) %>%
      group_by(location) %>%
      summarise(n_na_align_dates = sum(is.na(align_date))) %>% ungroup %>%
      filter(n_na_align_dates == 0) %>% pull(location) %>% unique
    
    df_train_use <- df_train_use %>% filter(location %in% c(locs1,locs2)) %>% 
      mutate(original_value = value)
    df_strata <- stratifier(df_train_use, response)
    df_with_lags <- make_data_with_lags(df_train_use, forecast_dates[itr], incidence_period, 
                                        ahead, response, features) %>%
      left_join(df_align, by = c("location", "time_value")) %>%
      left_join(df_strata, by = "location")
    
    df_point_preds_less_grim <- df_with_lags %>% filter(!strata) %>%
      local_lasso_daily_forecast_by_stratum(response, degree, bandwidth, forecast_dates[itr], 
                                            incidence_period, ahead, features, df_align, modeler)
    
    df_point_preds_more_grim <- df_with_lags %>% filter(strata) %>%
      local_lasso_daily_forecast_by_stratum(response, degree, bandwidth, forecast_dates[itr],
                                            incidence_period, ahead, features, df_align, modeler)

    point_preds_list[[itr]] <- bind_rows(df_point_preds_less_grim, df_point_preds_more_grim) %>%
      left_join(df_use %>% filter(variable_name == response) %>% select(location, time_value, value),
                by = c("location", "time_value")) %>%
      rename(original_value = value)
  }
  
  df_point_preds <- bind_rows(point_preds_list)
  df_bootstrap_preds <- bootstrapper(B, df_point_preds, forecast_date, incidence_period, ahead) %>%
    pivot_longer(-c(location, time_value), names_to = "replicate", values_to = "value") %>% # Put response back on original scale.
    group_by(location, replicate) %>%
    summarise(value = sum(pmax(value, 0))) %>% ungroup()
  
  preds_df <- df_bootstrap_preds %>% group_by(location) %>% 
    group_modify(~ data.frame(probs = covidhub_probs, quantiles = round(quantile(.x$value,covidhub_probs))))
  return(preds_df)
}

#' @importFrom magrittr %$%
#' @importFrom evalcast get_target_period
local_lasso_daily_forecast_by_stratum <- function(df_use, response, degree, bandwidth,forecast_date, 
                                                  incidence_period, ahead, features, df_align, modeler){
  response_name <- paste0(response,"_lag_0")
  locations <- df_use %>% filter(variable_name == response_name) %>% select(location) %>% distinct
  
  YX <- df_use %>% select(location, align_date, time_value, variable_name, value) %>% 
    filter(!is.na(align_date)) %>%
    pivot_wider(names_from = "variable_name", values_from = "value") %>%
    rename(response = response_name, date = align_date)
  YX_time_values <- YX %>% pull(time_value)
  YX <- YX %>% select(-time_value)
  
  for ( itr in 1:nrow(features) ){
    if ( is.na(features$lag[itr]) ){
      feature_name <- features$variable_name[itr]
    } else{
      feature_name <- paste0(features$variable_name[itr], "_lag_", features$lag[itr])
    }
    stopifnot(feature_name %in% names(YX))
    YX[[feature_name]] <- as.numeric(YX[[feature_name]])
  }

  target_dates <- evalcast::get_target_period(forecast_date, incidence_period, ahead) %$%
    seq(start, end, by = "days")
  dates <- df_align %>% filter(location %in% locations$location, time_value %in% target_dates) %>%
    pull(align_date) %>% unique

  preds <- list()
  for ( itr in 1:length(dates) ){
    YX_use <- YX %>% mutate(t = as.numeric(date - dates[itr])) # Add time relative to date.
    forecast_rows <- which(YX_use$date == dates[itr] & YX_time_values %in% target_dates) # right align date and right time value
    forecast_locs <- YX_use[forecast_rows,] %>% pull(location)
    forecast_time_values <- YX_time_values[forecast_rows]
    stopifnot(unique(forecast_locs) == forecast_locs)
    YX_use <- YX_use %>% select(-date)
    
    wts <- dnorm( YX_use$t  / bandwidth )
    t <- YX_use %>% pull(t)
    YX_use <- YX_use %>% select(-t)
    
    train_indices <- !is.na(YX_use$response) # response is NA only in the target period...
    X_train_test <- model_matrix(YX_use, features)
    X_train <- X_train_test[train_indices,,drop = F]
    Y_train <- (YX_use %>% pull(response))[train_indices]
    wts_train <- wts[train_indices]
    X_test <- X_train_test[forecast_rows,,drop = F] # Keep a one row matrix as a matrix.
    
    train_locs <- (YX_use %>% pull(location))[train_indices]
    train_t <- t[train_indices]
    fit <- modeler$fitter(Y = Y_train, X = X_train, wts = wts_train, offset = NULL, 
                          intercept = FALSE, locs = train_locs, t = train_t)
    preds[[itr]] <- data.frame(location = forecast_locs, time_value = forecast_time_values,
                               preds = modeler$predicter(fit  = fit, X = X_test, offset = NULL, locs = forecast_locs))
  }
  df_final <- expand_grid(locations, time_value = target_dates, strata = df_use$strata[1]) %>%
    left_join(bind_rows(preds), by = c("location", "time_value"))
  return(df_final)
}

#' @importFrom evalcast get_target_period
#' @import purrr
make_data_with_lags <- function(df_use, forecast_date, incidence_period, ahead, response, features){

  df_use <- df_use %>% filter(variable_name %in% c(features$variable_name, response))
  locations <- df_use %>% filter(variable_name == response) %>% select(location, geo_value) %>% 
    distinct %>% arrange(geo_value)
  target_period <- get_target_period(forecast_date, incidence_period, ahead)
  target_dates <- seq(target_period$start, target_period$end, by = "days")
  time_values <- unique(c(df_use %>% pull(time_value), target_dates))
  all_dates <- seq(min(time_values), max(time_values), by = 1)
  variables <- unique(c(response, features$variable_name))
  
  lags <- unique(features$lag)
  lag_functions <- lags %>% map(function(x) ~ na.locf(lag(., n = x, default = first(.))))
  names(lag_functions) <- paste0("lag_", lags)
  feature_names <- paste0(features$variable_name, "_lag_", features$lag)
  response_name <- paste0(response, "_lag_", 0)
  
  df_with_lags <- expand_grid(locations, time_value = all_dates, variable_name = variables) %>%
    left_join(df_use, by = c("location", "geo_value", "time_value", "variable_name")) %>%
    group_by(location, variable_name) %>%
    arrange(time_value) %>%
    mutate_at(.vars = vars(value), .funs = lag_functions) %>%
    ungroup() %>%
    rename(lag_0 = value) %>%
    pivot_longer(contains("lag_"), names_to = "lag", values_to = "value") %>% 
    filter(time_value %in% time_values) %>%
    mutate(variable_name = paste0(variable_name, "_", lag)) %>% 
    filter(variable_name %in% c(feature_names, response_name)) %>%
    select(-lag)

  return(df_with_lags)
}

#' @import covidcast
long_to_wide <- function(df){
  # Manipulate evalcast df to the wide format previously used during evalforecast era
  # This is a really hacky way to circumvent the issue while GitHub issue #269 is pending
  if ( nrow(unique(df %>% select(data_source, signal, geo_value, time_value))) < nrow(df) ){
    min_issue <- min(df$issue, na.rm = TRUE)
    df.tmp <- df %>% 
      mutate(issue = replace_na(issue, min_issue - 1)) %>%
      group_by(data_source, signal, geo_value, time_value) %>%
      top_n(1, wt = issue) %>% # NA only chosen if that's all there is
      ungroup %>%
      mutate(issue = na_if(issue, min_issue - 1)) # go back to NA
  }else{
    df.tmp <- df
  }
  match.string.1 <- with(df.tmp, paste0(data_source, "-", signal, geo_value, time_value))
  df <- df %>% mutate(variable_name = paste(data_source, signal, sep = "-"),
                      location = formatC(covidcast::state_census$STATE[match(toupper(df$geo_value),
                                                                             covidcast::state_census$ABBR)], 
                                         width = 2, flag = "0")) 
  # Need to open GitHub issue here
  # --- covidcast::aggregate_signals gets rid of the cumulative cases signal unless I break the df up like this
  # --- Maybe because the value column names are different character lengths?
  df1 <- df %>% filter(variable_name == "jhu-csse-deaths_7dav_incidence_num") %>% 
    aggregate_signals(format = "wide")
  df2 <- df %>% filter(variable_name == "jhu-csse-confirmed_7dav_incidence_num") %>% 
    aggregate_signals(format = "wide")
  df3 <- df %>% filter(variable_name == "jhu-csse-confirmed_cumulative_num") %>% 
    aggregate_signals(format = "wide")
  names(df1)[which(substr(names(df1),1,5) == "value")] <- "value"
  names(df2)[which(substr(names(df2),1,5) == "value")] <- "value"
  names(df3)[which(substr(names(df3),1,5) == "value")] <- "value"
  df <- bind_rows(df1, df2, df3)
  match.string.2 <- with(df, paste0(variable_name, geo_value, time_value))
  df$issue <- df.tmp$issue[match(match.string.2, match.string.1)]
  df <- df %>% select(location, geo_value, variable_name, value, time_value, issue) %>%
    mutate(geo_value = toupper(geo_value))
  df$value <- as.double(df$value)
  return(df)
}

#' @importFrom Matrix Matrix
model_matrix <- function(dat, features = NULL){
  # A wrapper around model.matrix,
  # allowing us to dynamically build the formula we would like to feed to model matrix.
  X_frame <- dat %>% select(-response)

  if ( length(unique(X_frame$location)) > 1 ){
    X_formula <- model_formula(features)
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

model_formula <- function(features){
  main_effect_features <- features %>% 
    mutate(feature_name = paste0(variable_name, "_lag_", lag)) %>%
    mutate(feature_name = if_else(grepl("-", feature_name), paste0("`", feature_name, "`"),
                                  feature_name)) %>% pull(feature_name)
  main_effect_chr <- paste0(main_effect_features, collapse = " + ")
  main_effect_chr <- paste0(main_effect_chr, "- 1")
  formula_chr <- paste0("~ ", main_effect_chr)
  return(as.formula(formula_chr))
}

make_cv_glmnet <- function(alpha = 1, fdev = 0, mnlam = 100, n_folds = 10){
  # Closure, allowing us to pass tuning parameters to cv.glmnet
  # Inputs:
  #   alpha: numeric between 0 and 1
  #   build_penalty_factor: function, taking as input the names of X and producing output which
  #                         can be passed to cv.glmnet as the penalty.factor argument
  #   fdev, mnlam: parameters to be passed to glmnet.control(). See help(glmnet.control) for details.
  #   n_folds: number of folds to use for cross-validation.
  
  cv_glmnet <- function(Y, X, wts, offset, locs, ...){
    stopifnot(is.character(locs))
    
    variable_names <- colnames(X)
    penalty_factor <- case_when(
      grepl("location", variable_names) ~ 1, # Penalize location-specific effects
      grepl(":", variable_names)        ~ 0, # Don't penalize interactions
      TRUE                             ~ 0 # Don't penalize intercept
    )
    if ( all(penalty_factor == 0) ){
      penalty_factor <- rep(1, length(penalty_factor))
    }
    
    # (2) Determine folds for cross validation.
    unique_locs <- unique(locs)
    stopifnot(length(unique_locs) >= n_folds) # Need something to hold out.
    fold_for_each_loc <- rep(1:n_folds, length.out = length(unique_locs))
    names(fold_for_each_loc) <- unique_locs
    fold_id <- sapply(locs, FUN = function(loc){which(names(fold_for_each_loc) == loc)})
    
    # (3) Fit our model.
    glmnet.control(fdev = fdev, mnlam = mnlam)
    cv.glmnet(x = X, y = Y, alpha = alpha, weights = wts, offset = offset,
              penalty.factor = penalty_factor, intercept = FALSE,
              nfolds = n_folds, foldid = fold_id, type.measure = "mse")
  }
}

make_predict_glmnet <- function(lambda_choice){
  # Closure, allowing us to pass parameters to predict.glmnet.
  # Inputs:
  #   lambda_choice: either "lambda.1se" or "lambda.min"
  predict_glmnet <- function(fit, X, offset, ...){
    stopifnot(is.character(lambda_choice))
    preds <- predict(fit, newx = X, newoffset = offset, s = lambda_choice)[,1]
    return(preds)
  }
}