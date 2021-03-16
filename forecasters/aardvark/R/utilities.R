#' @import evalcast
reformat_df <- function(df, column){
  df %>% select(names(df)[c(1, 2, column)]) %>%
    mutate(variable_name = strsplit(names(df)[column], ":")[[1]][2]) %>%
    rename(value = names(df)[column]) %>%
    select(geo_value, variable_name, value, time_value)
}

get_top_n_locations <- function(df, response, n){
  stopifnot(is.data.frame(df),
            is.character(response),
            is.numeric(n))
  stopifnot(c("geo_value",
              "time_value",
              "variable_name",
              "value") %in% names(df))
  stopifnot(response %in% df$variable_name)
  stopifnot(n > 0, n == round(n))
  df_use <- df %>% select(geo_value, time_value, variable_name, value)
  df_latest <- df_use %>%
    filter(variable_name == response) %>%
    group_by(geo_value, time_value) %>%
    top_n(n = 1, wt = time_value) %>%
    ungroup()
  top_n_locations <- df_latest %>%
    group_by(geo_value) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    top_n(n = !!n, wt = value) %>%
    pull(geo_value)
  return(top_n_locations)
}

kernel_smoother <- function(dat, h = 7, kern = "boxcar"){
  
  if ( kern == "boxcar" ){
    
    first_date <- min(dat %>% pull(time_value))
    last_date <- max(dat %>% pull(time_value))
    date_df <- data.frame(time_value = seq(first_date, last_date, by = "days"))
    full_df <- left_join(date_df, dat, by = c("time_value"))
    
    smoothed_dat <- full_df %>%
      group_by(geo_value) %>%
      arrange(time_value) %>%
      mutate(smoothed_value = rollmean(value, h, align = "right", fill = "extend")) %>%
      ungroup
    
    return(smoothed_dat)
  }
}

#' @importFrom evalcast get_target_period
#' @import purrr
make_data_with_lags <- function(df_use, forecast_date, incidence_period, ahead, response, features){
  df_use <- df_use %>% filter(variable_name %in% c(features$variable_name, response))
  geo_values <- df_use %>% 
    filter(variable_name == response) %>% 
    select(geo_value) %>% 
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
  
  df_with_lags <- expand_grid(geo_values, time_value = all_dates, variable_name = variables) %>%
    left_join(df_use, by = c("geo_value", "time_value", "variable_name")) %>%
    group_by(geo_value, variable_name) %>%
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
  if ( length(unique(X_frame$geo_value)) > 1 ){
    X_formula <- model_formula(features)
    X_train <- Matrix::Matrix( model.matrix(X_formula, X_frame), sparse = T)
  } else{
    if ( all(names(X_frame) == "geo_value") ){
      X_train <- matrix(0, ncol = 1, nrow = nrow(X_frame))
      colnames(X_train) <- "zero_col"
    } else{
      X_train <- data.matrix(X_frame %>% select(-geo_value))
    }
  }
  return(X_train)
}

model_formula <- function(features){
  features <- features %>% 
    mutate(feature_name = paste0(variable_name, "_lag_", lag)) %>%
    mutate(feature_name = if_else(grepl("-", feature_name), paste0("`", feature_name, "`"),
                                  feature_name)) %>% pull(feature_name)
  formula_chr <- paste0("~ ", paste0(features, collapse = " + "), "- 1")
  return(as.formula(formula_chr))
}

make_cv_glmnet <- function(){
  cv_glmnet <- function(Y, X, wts, locs, n_folds = 10, ...){
    stopifnot(is.character(locs))
    variable_names <- colnames(X)
    penalty_factor <- case_when( grepl("location", variable_names) ~ 1, TRUE ~ 0 )
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

#' @importFrom stats var
make_fv_glmnet_by_geo_value <- function(n_validation = 28){
  fv_glmnet_by_geo_value <- function(Y, X = NULL, wts = rep(1,length(Y)), locs, t){
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

make_predict_glmnet <- function(){
  predict_glmnet <- function(fit, X, ...){
    preds <- predict(fit, newx = X, s = "lambda.min")[,1]
    return(preds)
  }
}

make_predict_glmnet_by_geo_value <- function(){
  predict_glmnet_by_geo_value <- function(fit, X, locs){
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
