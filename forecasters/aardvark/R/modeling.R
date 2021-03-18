make_aardvark_forecaster <- function(response = NULL, 
                                     features = NULL, 
                                     geo_type_override = NULL){
  
  covidhub_probs <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
  
  aardvark_forecaster <- function(df, 
                                  forecast_date, 
                                  signals, 
                                  incidence_period = "epiweek",
                                  ahead, 
                                  geo_type){

    forecast_date <- ymd(forecast_date)
    target_period <- get_target_period(forecast_date, incidence_period, ahead)
    geo_type <- ifelse(geo_type_override == "nation", "nation", geo_type)
    alignment_variable <- features %>% 
      filter(grepl("confirmed_incidence_num", features %>% pull(variable_name))) %>% 
      pull(variable_name) %>% 
      unique
    threshold <- ifelse(geo_type == "county", 50, 5000)

    df <- df %>% aggregate_signals(format = "wide") 
    df_train <- lapply(X = 3:ncol(df), FUN = function(X) reformat_df(df, column = X)) %>% 
      bind_rows %>%
      distinct %>%
      mutate(value = as.double(value))
    
    if ( geo_type == "county" ){
      df_train <- df_train %>% filter(geo_value %in% get_top_n_locations(df_train, response, 200))
    }
    
    df_train_smoothed <- expand_grid(distinct(select(df_train, geo_value)),
                                     time_value = unique(df_train$time_value),
                                     variable_name = unique(df_train$variable_name)) %>%
      left_join(df_train, by = c("geo_value", "time_value", "variable_name")) %>%
      mutate(value = if_else(is.na(value), replace_na(value, 0), value)) %>%
      group_by(variable_name) %>%
      group_modify(~ kernel_smoother(.x)) %>% 
      rename(original_value = value, value = smoothed_value) %>%
      ungroup() 
    
    bootstrap_bandwidth <- 14
    train_forecast_dates <- forecast_date - rev(seq(7, bootstrap_bandwidth, by = 7) + (ahead - 1) * 7)
    forecast_dates <- c(train_forecast_dates, forecast_date)
    
    point_preds_list <- list()
    for ( itr in 1:length(forecast_dates) ){
      
      df_train_use <- df_train_smoothed %>% filter(time_value <= forecast_dates[itr] | is.na(time_value))
      df_align <- df_train_use %>% time_aligner(forecast_dates[itr], 
                                                alignment_variable = alignment_variable,
                                                ahead = ahead,
                                                threshold = threshold)
      df_train_use <- df_train_use %>% mutate(observed_value = value)
      df_with_lags <- make_data_with_lags(df_train_use, forecast_dates[itr], incidence_period, 
                                          ahead, response, features) %>%
        left_join(df_align, by = c("geo_value", "time_value"))
      
      point_preds_list[[itr]] <- df_with_lags %>%
        point_forecast_daily(response = response, 
                             bandwidth = 7,
                             forecast_date = forecast_dates[itr], 
                             incidence_period = incidence_period, 
                             ahead = ahead, 
                             features = features, 
                             df_align = df_align) %>%
        left_join(df_train_smoothed %>% filter(variable_name == response) %>% select(geo_value, time_value, value),
                  by = c("geo_value", "time_value")) %>%
        rename(observed_value = value)
    }
    
    ####
    
    df_point_preds <- bind_rows(point_preds_list)
    
    saveRDS(df_point_preds, file = "~/Desktop/df_point_preds.rds")
    
    df_bootstrap_preds <- gaussian_bootstrap_by_geo_value(df_point_preds, forecast_date, incidence_period, ahead) %>%
      pivot_longer(-c(geo_value, time_value), names_to = "replicate", values_to = "value") %>% 
      group_by(geo_value, replicate) %>%
      summarize(value = sum(pmax(value, 0)), .groups = "drop")
    df_preds <- df_bootstrap_preds %>% 
      group_by(geo_value) %>% 
      group_modify(~ data.frame(probs = covidhub_probs, quantiles = round(quantile(.x$value,covidhub_probs))))
    
    predictions <- expand_grid(unique(df_train_smoothed %>% select(geo_value)),
                               probs = covidhub_probs) %>% 
      left_join(df_preds, by = c("geo_value", "probs")) %>%
      mutate(quantiles = pmax(replace_na(quantiles, 0), 0), ahead = ahead) %>% 
      rename(quantile = probs, value = quantiles) %>%
      select(ahead, geo_value, quantile, value) %>% 
      mutate(ahead = as.integer(ahead)) %>% 
      arrange(geo_value)
    return(predictions)
  }
}

#' @importFrom magrittr %$%
#' @importFrom evalcast get_target_period
point_forecast_daily <- function(df_use, 
                                 response, 
                                 bandwidth, 
                                 forecast_date, 
                                 incidence_period, 
                                 ahead, 
                                 features, 
                                 df_align){
  
  model_fitter <- make_cv_glmnet()
  model_predicter <- make_predict_glmnet()
  modeler <- list(fitter = model_fitter, predicter = model_predicter)
  response_name <- paste0(response, "_lag_0")
  geo_values <- df_use %>% 
    filter(variable_name == all_of(response_name)) %>% 
    select(geo_value) %>% 
    distinct
  YX <- df_use %>% 
    select(geo_value, align_date, time_value, variable_name, value) %>% 
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
    filter(geo_value %in% geo_values$geo_value, time_value %in% target_dates) %>%
    pull(align_date) %>% 
    unique

  preds <- list()
  for ( itr in 1:length(dates) ){
    YX_use <- YX %>% mutate(t = as.numeric(date - dates[itr])) 
    forecast_rows <- which(YX_use$date == dates[itr] & YX_time_values %in% target_dates)
    forecast_locs <- YX_use[forecast_rows,] %>% pull(geo_value)
    forecast_time_values <- YX_time_values[forecast_rows]
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
    train_locs <- (YX_use %>% pull(geo_value))[train_indices]
    train_t <- t[train_indices]
    
    fit <- modeler$fitter(Y = Y_train, X = X_train, wts = wts_train, locs = train_locs, t = train_t)
    preds[[itr]] <- data.frame(geo_value = forecast_locs, time_value = forecast_time_values,
                               preds = modeler$predicter(fit  = fit, X = X_test, locs = forecast_locs))
  }
  df_final <- expand_grid(geo_values, time_value = target_dates) %>%
    left_join(bind_rows(preds), by = c("geo_value", "time_value"))
  return(df_final)
}
