make_aardvark_forecaster <- function(ahead = 1, incidence_period = c("epiweek", "day"),
                                     geo_type = c("state", "county", "national", "hrr", "msa"),
                                     backfill_buffer = 5, response = "jhu-csse_deaths_incidence_num", 
                                     features = NULL, bandwidth = 7, degree = 0, intercept = FALSE,
                                     stratifier, imputer = NULL, modeler = NULL, aligner = NULL,
                                     bootstrapper, B = 1000){

  covidhub_probs <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
  
  local_forecaster_with_shrinkage <- function(df, forecast_date, signals, incidence_period, ahead, geo_type){
    
    stopifnot(names(features) == c("variable_name","type","lag","offset","main_effect","impute"))
    stopifnot(names(modeler) == c("fitter", "predicter"))
    stopifnot(is.function(aligner))
    forecast_date <- lubridate::ymd(forecast_date)
    target_period <- get_target_period(forecast_date, incidence_period, ahead)
    
    saveRDS(df, file = "~/Desktop/aardvark_files/df_0.rds")
    
    # Manipulate evalcast df to the wide format previously used during evalforecast era
    # This is a really hacky way to circumvent the issue while GitHub issue #269 is pending
    if ( nrow(unique(df %>% select(data_source, signal, location, time_value))) < nrow(df) ){
      min_issue <- min(df$issue, na.rm = TRUE)
      df.tmp <- df %>% 
        mutate(issue = replace_na(issue, min_issue - 1)) %>%
        group_by(data_source, signal, location, time_value) %>%
        top_n(1, wt = issue) %>% # NA only chosen if that's all there is
        ungroup %>%
        mutate(issue = na_if(issue, min_issue - 1)) # go back to NA
    }else{
      df.tmp <- df
    }
    match.string.1 <- with(df.tmp, paste0(data_source, "-", signal, location, time_value))
    df$geo_value <- covidcast::state_census$ABBR[match(as.numeric(df$location), covidcast::state_census$STATE)]
    df <- df %>% mutate(variable_name = paste(data_source, signal, sep = "-")) %>%
      covidcast::aggregate_signals(format = "wide")
    match.string.2 <- with(df, paste0(variable_name, location, time_value))
    names(df)[which(substr(names(df),1,5) == "value")] <- "value"
    df$issue <- df.tmp$issue[match(match.string.2, match.string.1)]
    df <- df %>% select(location, geo_value, variable_name, value, time_value, issue)
    df$value <- as.double(df$value)
    df_train <- df
    rm(df, df.tmp); gc()
    stopifnot(c("location", "time_value", "issue") %in% names(df_train))
    
    saveRDS(df_train, file = "~/Desktop/aardvark_files/df_train_0.rds")

    # (1) Concentrate on the variables we need.
    alignment_variables <- environment(aligner)$variables
    saveRDS(alignment_variables, file = "~/Desktop/aardvark_files/alignment_variables.rds")
    df_train <- df_train %>% filter(variable_name %in% c(response, features$variable_name,
                                                         alignment_variables)) %>% distinct()
    saveRDS(df_train, file = "~/Desktop/aardvark_files/df_train_1.rds")

    # (2) Don't use any response data that hasn't solidified
    df_train <- filter(df_train, (variable_name != response) | (issue >= time_value + backfill_buffer) |
                                  is.na(issue)) # treat grandfathered data as solidified
    df_train <- df_train %>% select(-issue)
    
    saveRDS(df_train, file = "~/Desktop/aardvark_files/df_train_2.rds")

    # Stratification
    all_locs <- df_train %>% pull(location) %>% unique
    saveRDS(all_locs, file = "~/Desktop/aardvark_files/all_locs.rds")
    ## Ugly because too few responses to model as a continuous variable.
    locs_ugly_criterion1 <- df_train %>% 
      filter(variable_name == response) %>% group_by(location) %>% 
      summarize(n_response = sum(value, na.rm = T)) %>%
      filter(n_response <= 5) %>% 
      pull(location) %>% 
      unique()
    df_align <- aligner(df_train, forecast_date)
    
    saveRDS(locs_ugly_criterion1, file = "~/Desktop/aardvark_files/locs_ugly_criterion1.rds")
    saveRDS(df_align, file = "~/Desktop/aardvark_files/df_align.rds")
    
    ## Ugly because pandemic time hasn't yet begun for this location.
    locs_ugly_criterion2 <- setdiff(all_locs, df_align %>% 
                                      filter(!is.na(align_date)) %>% 
                                      pull(location) %>% unique())
    
    saveRDS(locs_ugly_criterion2, file = "~/Desktop/aardvark_files/locs_ugly_criterion2.rds")
    
    ## Ugly because we don't have the response variable for this location.
    locs_ugly_criterion3 <- setdiff(all_locs, df_train %>%
                                      filter(variable_name == response) %>%
                                      filter(!is.na(value)) %>% pull(location) %>% unique()) 
    locs_ugly <- unique(c(locs_ugly_criterion1, locs_ugly_criterion2, locs_ugly_criterion3))
    df_train_pretty <- df_train %>% filter( !(location %in% locs_ugly) )
    df_train_ugly <- df_train %>% filter(location %in% locs_ugly)
    
    saveRDS(locs_ugly_criterion3, file = "~/Desktop/aardvark_files/locs_ugly_criterion3.rds")
    saveRDS(df_train_pretty, file = "~/Desktop/aardvark_files/df_train_pretty.rds")
    saveRDS(df_train_ugly, file = "~/Desktop/aardvark_files/df_train_ugly.rds")
    
    # Predict.
    ## (1) Prepare data frame to hold predictions.
    df_all <- expand_grid(location = unique(df_train$location), probs = covidhub_probs)
    
    saveRDS(df_all, file = "~/Desktop/aardvark_files/df_all.rds")
    
    ## (2) Fit model and issue predictions for non-ugly locations.
    df_preds_pretty <- local_lasso_daily_forecast(df_train_pretty, response, degree, bandwidth,
                                                  forecast_date, incidence_period, ahead,
                                                  imputer, stratifier, aligner, modeler, 
                                                  bootstrapper, B, covidhub_probs,
                                                  features, intercept, alignment_variable)
    
    saveRDS(df_preds_pretty, file = "~/Desktop/aardvark_files/df_preds_pretty.rds")
    
    ## (3) Fit model and issue predictions for ugly locations.
    df_point_preds_ugly <- df_train_ugly %>% 
      filter(variable_name == response) %>%
      group_by(location) %>%
      summarise(preds = pmax(mean(value, na.rm = T), 0)) %>%
      ungroup()
    df_preds_ugly <- expand_grid(df_point_preds_ugly, probs = covidhub_probs) %>%
      mutate(quantiles = qnbinom(p = probs, mu = preds, size = 0.25)) %>%
      select(-preds)
    non_zero_locs <- filter(df_point_preds_ugly, preds > 0) %>% pull(location) %>% unique()
    df_preds_ugly <- df_preds_ugly %>%
      mutate(quantiles = if_else(
        probs >= 0.95 & location %in% non_zero_locs,
        pmax(quantiles, 2),
        quantiles
      ))
    
    saveRDS(df_preds_ugly, file = "~/Desktop/aardvark_files/df_preds_ugly.rds")
    
    ## (4) Combine
    df_preds <- bind_rows(df_preds_ugly, df_preds_pretty)
    saveRDS(df_preds, file = "~/Desktop/aardvark_files/df_preds.rds")
    
    ## (5) Replace NA and negative predictions by 0.
    predictions <- left_join(df_all, df_preds, by = c("location", "probs")) %>%
      mutate(quantiles = pmax(replace_na(quantiles, 0), 0))
    predictions$ahead <- ahead
    predictions$geo_value <- covidcast::state_census$ABBR[match(as.numeric(predictions$location), covidcast::state_census$STATE)]
    saveRDS(predictions, file = "~/Desktop/aardvark_files/predictions.rds")
    return(predictions)
  }
}

local_lasso_daily_forecast <- function(df_use, response, degree, bandwidth,
                                       forecast_date, incidence_period, ahead,
                                       imputer, stratifier, aligner, modeler,
                                       bootstrapper, B, covidhub_probs,
                                       features, intercept, alignment_variable){
  
  # Use our bootstrapper information to determine our training forecast dates
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
    df_train_use <- filter(df_use, time_value <= forecast_date_ii | is.na(time_value))
    
    # (1) Preprocess
    ## (A) Only keep variables for which we've observed a non-zero response.
    response_locs <- unique(df_train_use %>% filter(variable_name == response & value > 0) %>% pull(location))
    
    ## (B) Only keep locations for which aligned time is never NA in the target period
    df_align <- aligner(df_train_use, forecast_date_ii)
    target_dates <-  evalcast::get_target_period(forecast_date_ii, incidence_period, ahead) %$%
      seq(start, end, by = "days")
    pretty_locs <- unique(df_align %>% 
                            filter(time_value %in% target_dates) %>% # dates in the target period
                            group_by(location) %>%
                            summarise(n_na_align_dates = sum(is.na(align_date))) %>% # number of NA align_dates
                            ungroup %>%
                            filter(n_na_align_dates == 0) %>% 
                            pull(location))
    
    if ( length(response_locs) == 0 | length(pretty_locs) == 0 ){
      warning(paste0("Training forecast date", forecast_date_ii, "has no pretty locations; moving on."))
      point_preds_list[[ii]] <- NULL
      next
    }
    
    df_train_use <- filter(df_train_use, location %in% response_locs & location %in% pretty_locs)
    df_original_response <- df_train_use %>% filter(variable_name == response)
    
    ## (C) Impute/smooth
    if ( !is.null(imputer) ){
      # (I) Variables to impute.
      impute_variables <- unique(c(response, features %>% filter(impute) %>% pull(variable_name)))
      df_impute <- filter(df_train_use, variable_name %in% impute_variables)
      df_no_impute <- filter(df_train_use, !(variable_name %in% impute_variables))
      
      # (II) Make sure all variables are present in the data frame.
      location_df <- distinct(select(df_impute, c(location)))
      df_empty <- expand_grid(location_df,
                              time_value = unique(df_impute$time_value),
                              variable_name = unique(df_impute$variable_name))
      df_impute_all <- left_join(df_empty, df_impute, by = c("location", "time_value", "variable_name"))
      
      # (III) Impute NA by 0.
      warning("You are treating NA as 0 in the smoothing step.")
      df_impute_all_no_na <- df_impute_all %>%
        mutate(value = if_else(variable_name %in% impute_variables & is.na(value),
                               replace_na(value,0),
                               value))
      
      # (IV) Smooth out our data.
      df_imputed <- df_impute_all_no_na %>%
        group_by(variable_name) %>%
        rename(date = time_value) %>% # adopt our old convention
        group_modify(~ if(.y$variable_name %in% impute_variables) imputer(.x) else .x) %>% # impute
        rename(original_value = value, value = imputed_value, time_value = date) %>%
        ungroup() # go back to the new convention
      
      # (V) Add back in variables which were not supposed to be imputed
      df_train_use <- bind_rows(df_imputed, df_no_impute)
    } else{
      df_train_use <- df_train_use %>% mutate(original_value = value)
    }
    
    # (2) Add lagged variables as additional features
    #     and add rows for all dates (including training period dates and target period dates)
    #     on which we would like forecasts.
    df_with_lags <- make_data_with_lags(df_train_use, forecast_date_ii, incidence_period, 
                                        ahead, response, features)
    
    # (3) Add columns "strata" and "align_date".
    ## (A) Compute strata
    df_strata <- stratifier(df_train_use, response)
    
    ## (B) Augment df_with_lags
    df_with_lags <- left_join(df_with_lags, df_align, by = c("location", "time_value")) %>%
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
                                              features, intercept, df_align, modeler)
    }
    
    dat_good <- filter(df_with_lags, strata)
    if ( nrow(dat_good) == 0 ){
      warnings("No data in good strata.")
      df_point_preds_good <- NULL
    } else{
      df_point_preds_good <- dat_good %>% 
        local_lasso_daily_forecast_by_stratum(response, degree, bandwidth, forecast_date_ii, 
                                              incidence_period, ahead, features, intercept, df_align, modeler)
    }
    
    # (5) Prepare output, by joining strata and adding original value of the response
    df_point_preds_ii <- bind_rows(df_point_preds_bad, df_point_preds_good) %>%
      left_join(df_use %>% 
                  filter(variable_name == response) %>%
                  select(location, time_value, value),
                by = c("location","time_value")) %>%
      rename(original_value = value)
    
    point_preds_list[[ii]] <- df_point_preds_ii
  }
  df_point_preds <- bind_rows(point_preds_list)
  
  # Monte Carlo estimate of distribution of response
  df_bootstrap_preds <- bootstrapper(B, df_point_preds, forecast_date, incidence_period, ahead)
  
  # Put response back on original scale.
  df_bootstrap_preds <- df_bootstrap_preds %>%
    pivot_longer(-c(location, time_value), 
                 names_to = "replicate", values_to = "value")
  
  # If we need to sum over multiple dates...
  df_bootstrap_preds <- df_bootstrap_preds %>%
    group_by(location, replicate) %>%
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
local_lasso_daily_forecast_by_stratum <- function(df_use, response, degree, bandwidth,
                                                  forecast_date, incidence_period, ahead,
                                                  features, intercept, df_align, modeler){
  # Returns a data frame with point predictions for each (location, time_value)
  # pair satisfying location in "good" strata, and 
  #                 time_value in target_period.
  # Inputs:
  #   df_use: data used to make forecasts. At minimum, should have the columns
  #           location, time_value, variable_name, original_value, value,
  #           and align_date.
  
  # (1) Preamble
  response_name <- paste0(response,"_lag_0")
  locations <- distinct(df_use %>% filter(variable_name == response_name) %>% select(location))
  
  # (2) Pivot wider, for model matrix form.
  YX <- df_use %>%
    select(location, align_date, time_value, variable_name, value) %>% 
    filter(!is.na(align_date)) %>%
    pivot_wider(names_from = "variable_name", values_from = "value") %>%
    rename(response = response_name, date = align_date)
  
  ## Save reference date information, so we know which are target rows.
  YX_time_values <- YX %>% pull(time_value)
  YX <- YX %>% select(-time_value)
  
  # (3) Assign feature the right type.
  for ( ii in 1:nrow(features) ){
    # (A) Get the right name for the feature (i.e. the name it will have in YX)
    if ( is.na(features$lag[ii]) ){
      feature_name <- features$variable_name[ii]
    } else{
      feature_name <- paste0(features$variable_name[ii], "_lag_", features$lag[ii])
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
    stop("The following features: ", paste0(na_features, collapse = " and "),
         " have NA values.")
  }
  
  # (This check makes sure our stratifier has screened out any variables for which
  #  all data has happened on an NA date; recall that align_date might be NA.)
  stopifnot(length(setdiff(locations %>% pull(location),
                           YX %>% pull(location) %>% unique)) == 0)
  
  # (3) If we have specified an offset, extract it.
  offset <- NULL
  if ( !is.null(features) ){
    offset_df <- filter(features, offset)
    feature_df <- filter(features, !offset)
    stopifnot(nrow(offset_df) <= 1)
    if ( nrow(offset_df) > 0 ){
      offset_variable <- paste0(offset_df$variable_name, "_lag_", offset_df$lag)
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
  target_dates <- evalcast::get_target_period(forecast_date, incidence_period, ahead) %$%
    seq(start, end, by = "days")
  dates <- unique(df_align %>% 
                    filter(location %in% locations$location, time_value %in% target_dates) %>%
                    pull(align_date))
  
  ## (B) One fit per time
  preds <- list()
  for ( ii in 1:length(dates) ){
    # (I) Add time relative to date.
    YX_use <- YX %>% mutate(t = as.numeric(date - dates[ii]))
    
    # (II) Remember which rows we should predict
    forecast_rows <- which(YX_use$date == dates[ii] & # right align date
                             YX_time_values %in% target_dates) # right reference date
    forecast_locs <- YX_use[forecast_rows,] %>% pull(location)
    forecast_time_values <- YX_time_values[forecast_rows]
    stopifnot(unique(forecast_locs) == forecast_locs)
    
    # (III) We don't need date any more.
    YX_use <- YX_use %>% select(-date)
    
    # (IV) Add polynomials in t.
    if ( degree > 0 ){
      t_use <- data.frame(poly(YX_use$t, degree = degree))
      names(t_use) <- paste0("t", 1:degree)
      YX_use <- bind_cols(YX_use, t_use)
    }
    
    # (V) Compute weights as a function of t
    wts <- dnorm( YX_use$t  / bandwidth )
    
    # (VI) Strip t from YX_use, but hold on to it as a vector
    t <- YX_use %>% pull(t)
    YX_use <- YX_use %>% select(-t)
    
    # (VII) Build training and test sets
    train_indices <- !is.na(YX_use$response) # response is NA only in the target period...
    X_train_test <- model_matrix(YX_use, intercept, features)
    X_train <- X_train_test[train_indices,,drop = F]
    Y_train <- (YX_use %>% pull(response))[train_indices]
    wts_train <- wts[train_indices]
    offset_train <- offset[train_indices]
    X_test <- X_train_test[forecast_rows,,drop = F] # Keep a one row matrix as a matrix.
    offset_test <- offset[forecast_rows]
    
    # (VIII) Fit our model
    train_locs <- (YX_use %>% pull(location))[train_indices]
    train_t <- t[train_indices]
    fit <- modeler$fitter(Y = Y_train, X = X_train, wts = wts_train, offset = offset_train, 
                          intercept = intercept, locs = train_locs, t = train_t)
    
    # (IX) Predict using our model.
    preds_ii <- data.frame(location = forecast_locs, time_value = forecast_time_values,
                           preds = modeler$predicter(fit  = fit, X = X_test, offset = offset_test,
                                                     locs = forecast_locs))
    preds[[ii]] <- preds_ii
  }
  df_preds <- bind_rows(preds)
  
  # (5) Prepare output
  df_empty <- expand_grid(locations,time_value = target_dates, strata = df_use$strata[1])
  df_final <- left_join(df_empty,df_preds, by = c("location", "time_value"))
  return(df_final)
}

#' @importFrom evalcast get_target_period
#' @import purrr
make_data_with_lags <- function(df_use, forecast_date, incidence_period, ahead, response, features){
  ## This function assembles all the data we will need for training and predicting.
  ## This means **I guarantee** the output of this function should have 
  ## an entry for each (variable_name, location, date) triple
  ## in either my training or test period. 
  ## This guarantee should hold for both temporal and non-temporal variables,
  ## which we do by treating non-temporal variables like a time series which only ever has
  ## one value.
  
  # (1) Separate temporal and non-temporal variables
  df_temporal <- filter(df_use, !is.na(time_value))
  df_non_temporal <- filter(df_use, is.na(time_value))
  
  # (2) Get all the locations and dates we need.
  locations <- distinct(df_use %>% filter(variable_name == response) %>% select(location))
  target_period <- get_target_period(forecast_date, incidence_period, ahead)
  target_dates <- seq(target_period$start, target_period$end, by = "days")
  time_values <- unique(c(df_temporal %>% pull(time_value), target_dates))
  
  # (3) Build df for non-temporal variables.
  non_temporal_vars <- intersect(unique(df_non_temporal$variable_name), 
                                 c(response, features$variable_name))
  if ( length(non_temporal_vars) > 0 ){
    df_non_temporal_all <- expand_grid(locations,
                                       time_value = time_values,
                                       variable_name = non_temporal_vars) %>%
      left_join(df_non_temporal %>% select(-time_value), 
                by = c("location","variable_name"))
  } else{
    df_non_temporal_all <- NULL
  }
  
  # (4) Build df for temporal variables.
  ## (A) All possible location, date, variable_name triples.
  all_dates <- seq(min(time_values), max(time_values), by = 1)
  temporal_vars <- intersect(unique(df_temporal$variable_name), c(response, features$variable_name))
  stopifnot(length(temporal_vars) > 0)
  df_temporal_all <- expand_grid(locations, time_value = all_dates, variable_name = temporal_vars) %>%
    left_join(df_temporal, by = c("location","time_value","variable_name"))
  
  ## (c) Add lags.
  lags <- setdiff( unique(features$lag), NA )
  if ( !is.null(lags) ){
    stopifnot(is.numeric(lags))
    
    # Lags for all variables.
    lag_functions <- lags %>% 
      map(function(x) ~ 
            na.locf(lag(., n = x, default = first(.))))
    names(lag_functions) <- paste0("lag_", lags)
    
    df_temporal_all_with_lags <- df_temporal_all %>%
      group_by(location, variable_name) %>%
      arrange(time_value) %>%
      mutate_at(vars(value),.funs = lag_functions) %>%
      ungroup() %>%
      rename(lag_0 = value) %>%
      pivot_longer(contains("lag_"),
                   names_to = "lag",values_to = "value")
    
    # Tidy up rows, by just selecting the dates we want.
    df_temporal_all_with_lags <- df_temporal_all_with_lags %>%
      filter(time_value %in% time_values)
    
    # Tidy up columns
    df_temporal_all_with_lags <- df_temporal_all_with_lags %>%
      mutate(variable_name = paste0(variable_name, "_", lag)) %>%
      select(-lag)
    
    # Just select the ones we want. 
    feature_names <- paste0(features$variable_name, "_lag_", features$lag)
    response_name <- paste0(response, "_lag_", 0)
    df_temporal_with_lags <- df_temporal_all_with_lags %>% 
      filter(variable_name %in% c(feature_names, response_name))
  } else{
    df_temporal_with_lags <- df_temporal_all %>%
      mutate(variable_name = paste0(variable_name, "_lag_0"))
  }
  df_with_lags <- bind_rows(df_temporal_with_lags, df_non_temporal_all)
  return(df_with_lags)
}

#' @importFrom Matrix Matrix
model_matrix <- function(dat, intercept = TRUE, features = NULL){
  # A wrapper around model.matrix,
  # allowing us to dynamically build the formula we would like to feed to model matrix.
  X_frame <- dat %>% select(-response)

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
  # A function to create a formula with the main effects, interactions, and intercepts
  
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
  
  # (2) Create the interaction part of the formula
  interaction_chr <- NULL
  
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
  #   fdev, mnlam: parameters to be passed to glmnet.control(). See help(glmnet.control) for details.
  #   n_folds: number of folds to use for cross-validation.
  
  cv_glmnet <- function(Y, X, wts, offset, intercept, locs, ...){
    stopifnot(is.character(locs))
    
    # (1) Penalize interactions, either with locations or otherwise.
    penalty_factor <- build_penalty_factor(colnames(X))
    if ( all(penalty_factor == 0) ){
      # When nothing is being penalized, penalize everything equally.
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
    cv.glmnet(x = X, y = Y, alpha = alpha, weights = wts, offset = offset,
              penalty.factor = penalty_factor, intercept = intercept,
              nfolds = n_folds, foldid = fold_id,
              type.measure = "mse")
  }
}

make_predict_glmnet <- function(lambda_choice){
  # Closure, allowing us to pass parameters to predict.glmnet.
  # Inputs:
  #   lambda_choice: either "lambda.1se" or "lambda.min"
  predict_glmnet <- function(fit, X, offset,...){
    stopifnot(is.character(lambda_choice))
    preds <- predict(fit, newx = X, newoffset = offset, s = lambda_choice)[,1]
    return(preds)
  }
}