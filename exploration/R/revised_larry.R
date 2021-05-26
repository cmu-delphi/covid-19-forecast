model_dfs <- function(lagged_df, ahead, training_window_size) {
  out <- list()
  
  # make sure the response columns are unique
  responses_at_ahead <- lagged_df %>%
    select(tidyselect::starts_with(sprintf("response+%i:", ahead))) %>%
    ncol()
  assert_that(responses_at_ahead == 1,
              msg=paste("multiple responses at ahead =",ahead))
  
  
  # Find the last possible date of training data
  response_end_date <- lagged_df %>%
    select(time_value, tidyselect::starts_with(sprintf("response+%i:", ahead))) %>%
    tidyr::drop_na() %>%
    summarize(max(time_value)) %>%
    pull()
  train_end_date <- min(max(lagged_df$time_value), response_end_date)
  
  out$train_df <- lagged_df %>%
    filter(between(time_value,
                   train_end_date - training_window_size + 1,
                   train_end_date)) %>%
    select(geo_value, 
           starts_with(sprintf("response+%i", ahead)), 
           starts_with("value"))
  
  # Prediction matrices
  out$predict_x <- lagged_df %>%
    filter(time_value == max(time_value)) %>%
    select(geo_value, tidyselect::starts_with("value"))
  out$train_end_date <- train_end_date
  
  return(out)
}


larrys_anteater <- function(df_list, 
                            forecast_date, 
                            training_window_size = 28,
                            incidence_period = c("epiweek", "day"),
                            ahead = 1:4,
                            lags = 0,
                            tau = evalcast::covidhub_probs(),
                            featurize = NULL) {
  

  # data pivoting -----------------------------------------------------------
  if (class(df_list)[1] != "list") df_list <- list(df_list)
  nsigs <- length(df_list)
  incidence_period <- match.arg(incidence_period)
  dt <- animalia:::lag_processor(lags, nsigs) # make lags a list, perform checks
  ahead_in_days <- purrr::map_dbl(
    ahead,  ~evalcast::get_target_ahead(forecast_date, incidence_period, .x))
  dt[[1]] <- c(dt[[1]], ahead_in_days)   
  df_wide <- covidcast::aggregate_signals(df_list, dt = dt, format = "wide")
  
  if (!is.null(featurize)) df_wide <- featurize(df_wide)
  
  # rename response columns -------------------------------------------------
  response_cols <- stringr::str_detect(names(df_wide), "value\\+[1-9]")
  names(df_wide)[response_cols] <- stringr::str_replace(
    names(df_wide)[response_cols], "value", "response")
  names(df_wide)[response_cols] <- stringr::str_replace(
    names(df_wide)[response_cols], "[0-9]+", as.character(ahead))
  
  # Main loop over ahead values, fit model, make predictions ----------------
  result <- list()
  for (i in seq_along(ahead)) {
    
    mats <- model_dfs(df_wide, ahead[i], training_window_size)
    train_list <- split(mats$train_df %>% select(-geo_value), 
                        mats$train_df$geo_value) 
    test_list <- split(mats$predict_x %>% select(-geo_value), 
                       mats$predict_x$geo_value)
    form <- as.formula(paste0("`", names(mats$train_df)[2],"`", " ~ ", "."))
    fitter <- function(df, newdata) {
      fit <- lm(form, data = df)
      eq <- quantile(residuals(fit), tau)
      predict(fit, newdata) + eq
    }
    result[[i]] <- purrr::map2_dfr(train_list, test_list, fitter) %>%
      bind_cols(geo_value = mats$predict_x$geo_value) %>%
      mutate(ahead = ahead[i]) %>%
      pivot_longer(-c(ahead, geo_value), names_to = "quantile") %>%
      mutate(quantile = as.numeric(str_extract(quantile,"\\d+\\.?\\d*"))/100) %>%
      relocate(ahead, geo_value, quantile, value)
  }
  
  return(bind_rows(result))
}
