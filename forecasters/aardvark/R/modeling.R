make_aardvark_forecaster <- function(response = NULL, features = NULL, smoother = NULL, 
                                     aligner = NULL, modeler = NULL, bandwidth = 7,
                                     bootstrapper = NULL){
  
  covidhub_probs <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
  local_forecaster_with_shrinkage <- function(df, forecast_date, signals, incidence_period = c("epiweek","day"),
                                              ahead, geo_type){

    forecast_date <- ymd(forecast_date)
    incidence_period <- match.arg(incidence_period)
    target_period <- get_target_period(forecast_date, incidence_period, ahead)
    alignment_variable <- environment(aligner)$alignment_variable

    df <- df %>% aggregate_signals(format = "wide") 
    
    df_train <- lapply(X = 3:ncol(df), FUN = function(X) reformat_df(df, column = X)) %>% 
      bind_rows %>%
      distinct %>%
      mutate(value = as.double(value))
    
    df_train_smoothed <- expand_grid(distinct(select(df_train, location)),
                                     time_value = unique(df_train$time_value),
                                     variable_name = unique(df_train$variable_name)) %>%
      left_join(df_train, by = c("location", "time_value", "variable_name")) %>%
      mutate(value = if_else(is.na(value), replace_na(value, 0), value)) %>%
      group_by(variable_name) %>%
      group_modify(~ smoother(.x)) %>% 
      rename(original_value = value, value = smoothed_value) %>%
      ungroup() 
    
    if ( geo_type == "nation" ){
      df_train_smoothed <- df_train_smoothed %>%
        select(-c(location, geo_value)) %>%
        group_by(variable_name, time_value) %>%
        summarise(original_value = sum(original_value), value = sum(value), .groups = "drop") %>%
        mutate(location = "00", geo_value = "us")
    }

    df_preds <- local_lasso_daily_forecast(df_train_smoothed, response, bandwidth, forecast_date, 
                                           incidence_period, ahead, smoother, aligner, 
                                           modeler, bootstrapper, covidhub_probs, features, 
                                           alignment_variable)
    
    predictions <- expand_grid(unique(df_train_smoothed %>% select(location, geo_value)),
                               probs = covidhub_probs) %>% 
      left_join(df_preds, by = c("location", "probs")) %>%
      mutate(quantiles = pmax(replace_na(quantiles, 0), 0), ahead = ahead) %>% 
      rename(quantile = probs, value = quantiles) %>%
      select(ahead, geo_value, quantile, value) %>% 
      mutate(ahead = as.integer(ahead)) %>% 
      arrange(geo_value)
    return(predictions)
  }
}

#' @importFrom magrittr %$%
local_lasso_daily_forecast <- function(df_use, response, bandwidth, forecast_date, 
                                       incidence_period, ahead, smoother, aligner, 
                                       modeler, bootstrapper, covidhub_probs, features, 
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
    df_align <- aligner(df_train_use, forecast_dates[itr])
    df_train_use <- df_train_use %>% mutate(observed_value = value)
    df_with_lags <- make_data_with_lags(df_train_use, forecast_dates[itr], incidence_period, 
                                        ahead, response, features) %>%
      left_join(df_align, by = c("location", "time_value"))
    point_preds_list[[itr]] <- df_with_lags %>%
      local_lasso_daily_forecast_by_stratum(response, bandwidth, forecast_dates[itr], 
                                            incidence_period, ahead, features, df_align, modeler) %>%
      left_join(df_use %>% filter(variable_name == response) %>% select(location, time_value, value),
                by = c("location", "time_value")) %>%
      rename(observed_value = value)
  }
  
  df_point_preds <- bind_rows(point_preds_list)
  df_bootstrap_preds <- bootstrapper(df_point_preds, forecast_date, incidence_period, ahead) %>%
    pivot_longer(-c(location, time_value), names_to = "replicate", values_to = "value") %>% 
    group_by(location, replicate) %>%
    summarize(value = sum(pmax(value, 0)), .groups = "drop")
  preds_df <- df_bootstrap_preds %>% 
    group_by(location) %>% 
    group_modify(~ data.frame(probs = covidhub_probs, quantiles = round(quantile(.x$value,covidhub_probs))))
  return(preds_df)
}

#' @importFrom magrittr %$%
#' @importFrom evalcast get_target_period
local_lasso_daily_forecast_by_stratum <- function(df_use, response, bandwidth, forecast_date, 
                                                  incidence_period, ahead, features, df_align, modeler){
  response_name <- paste0(response, "_lag_0")
  locations <- df_use %>% 
    filter(variable_name == all_of(response_name)) %>% 
    select(location, geo_value) %>% 
    distinct
  YX <- df_use %>% 
    select(location, align_date, time_value, variable_name, value) %>% 
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

  target_dates <- get_target_period(forecast_date, incidence_period, ahead) %$%
    seq(start, end, by = "days")
  dates <- df_align %>% 
    filter(location %in% locations$location, time_value %in% target_dates) %>%
    pull(align_date) %>% 
    unique

  preds <- list()
  for ( itr in 1:length(dates) ){
    YX_use <- YX %>% mutate(t = as.numeric(date - dates[itr])) 
    forecast_rows <- which(YX_use$date == dates[itr] & YX_time_values %in% target_dates)
    forecast_locs <- YX_use[forecast_rows,] %>% pull(location)
    forecast_time_values <- YX_time_values[forecast_rows]
    stopifnot(unique(forecast_locs) == forecast_locs)
    YX_use <- YX_use %>% select(-date)
    wts <- dnorm( YX_use$t  / bandwidth )
    t <- YX_use %>% pull(t)
    YX_use <- YX_use %>% select(-t)
    train_indices <- !is.na(YX_use$response)
    X_train_test <- model_matrix(YX_use, features)
    X_train <- X_train_test[train_indices,,drop = F]
    Y_train <- (YX_use %>% pull(response))[train_indices]
    wts_train <- wts[train_indices]
    X_test <- X_train_test[forecast_rows,,drop = F]
    train_locs <- (YX_use %>% pull(location))[train_indices]
    train_t <- t[train_indices]
    
    check <- try(fit <- modeler$fitter(Y = Y_train, X = X_train, wts = wts_train, locs = train_locs, t = train_t))
    preds[[itr]] <- data.frame(location = forecast_locs, time_value = forecast_time_values,
                               preds = modeler$predicter(fit  = fit, X = X_test, locs = forecast_locs))
  }
  df_final <- expand_grid(locations, time_value = target_dates) %>%
    left_join(bind_rows(preds), by = c("location", "time_value"))
  return(df_final)
}

#' @import evalcast
reformat_df <- function(df, column){
  df %>% select(names(df)[c(1, 2, column)]) %>%
    mutate(variable_name = strsplit(names(df)[column], ":")[[1]][2]) %>%
    rename(value = names(df)[column]) %>% mutate(location = evalcast:::abbr_2_fips(geo_value)) %>%
    select(location, geo_value, variable_name, value, time_value)
}

#' @importFrom evalcast get_target_period
#' @import purrr
make_data_with_lags <- function(df_use, forecast_date, incidence_period, ahead, response, features){
  df_use <- df_use %>% filter(variable_name %in% c(features$variable_name, response))
  locations <- df_use %>% 
    filter(variable_name == response) %>% 
    select(location, geo_value) %>% 
    distinct %>% 
    arrange(geo_value)
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

model_matrix <- function(dat, features = NULL){
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

make_cv_glmnet <- function(){
  cv_glmnet <- function(Y, X, wts, locs, n_folds = 10, ...){
    stopifnot(is.character(locs))
    variable_names <- colnames(X)
    penalty_factor <- case_when(
      grepl("location", variable_names) ~ 1,
      TRUE                             ~ 0 
    )
    
    if ( all(penalty_factor == 0) ){
      penalty_factor <- rep(1, length(penalty_factor))
    }
  
    unique_locs <- unique(locs)
    fold_for_each_loc <- rep(1:n_folds, length.out = length(unique_locs))
    names(fold_for_each_loc) <- unique_locs
    fold_id <- sapply(locs, FUN = function(loc){which(names(fold_for_each_loc) == loc)})
  
    glmnet.control(fdev = 0, mnlam = 100)
    cv.glmnet(x = X, y = Y, alpha = 1, weights = wts, offset = NULL,
              penalty.factor = penalty_factor, intercept = FALSE,
              nfolds = n_folds, foldid = fold_id, type.measure = "mae")
  }
}

make_predict_glmnet <- function(){
  predict_glmnet <- function(fit, X, ...){
    preds <- predict(fit, newx = X, s = "lambda.min")[,1]
    return(preds)
  }
}

#' @importFrom stats var
make_fv_glmnet_by_location <- function(n_validation = 14){
  fv_glmnet_by_location <- function(Y, X = NULL, wts = rep(1,length(Y)), locs, t){
    fits <- list()
    for ( loc in unique(locs) ){
      loc_indices <- which(locs %in% loc)
      Y_loc <- Y[loc_indices]
      X_loc <- X[loc_indices,]
      wts_loc <- wts[loc_indices]
      t_loc <- t[loc_indices]
      train_indices <- which( !(order(t_loc, decreasing = T) %in% 1:n_validation) )
      validation_indices <- which ( order(t_loc, decreasing = T) %in% 1:n_validation )
      Y_loc_train <- Y_loc[train_indices]
      X_loc_train <- X_loc[train_indices,]
      wts_loc_train <- wts_loc[train_indices]
      Y_loc_validation <- Y_loc[validation_indices]
      X_loc_validation <- X_loc[validation_indices,]
      wts_loc_validation <- wts_loc[validation_indices]
      glmnet.control(fdev = 0, mnlam = 100)
      candidate_fits <- glmnet(x = X_loc_train, y = Y_loc_train, alpha = 1, weights = wts_loc_train, intercept = TRUE, nlambda = 100)
      error_validation_set <- colMeans( abs((Y_loc_validation - predict(candidate_fits, newx = X_loc_validation)) ))
      optimal_lambda <- candidate_fits$lambda[which.min(error_validation_set)]
      optimal_lambda <- ifelse(is.na(optimal_lambda), 0, optimal_lambda) 
      fits[[as.character(loc)]] <- glmnet(x = X_loc, y = Y_loc, alpha = 1, weights = wts_loc, intercept = TRUE, lambda = optimal_lambda)
    }
    return(fits)
  }
}

make_predict_glmnet_by_location <- function(){
  predict_glmnet_by_location <- function(fit, X, locs){
    preds <- numeric(length(locs))
    names(preds) <- as.character(locs)
    for ( loc in locs ){
      loc_indices <- which(locs %in% loc)
      loc_chr <- as.character(loc)
      fit_loc <- fit[[loc_chr]]
      X_loc <- X[loc_indices,,drop = F]
      preds[loc_chr] <- predict(fit_loc, newx = X_loc)[1]
    }
    return(preds)
  }
}
