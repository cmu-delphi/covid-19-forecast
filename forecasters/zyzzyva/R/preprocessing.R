#' @include modeling.R transformation.R
NULL

#' Add lagged columns to base_df. We make all possible lags,
#' and then select the ones we will use afterwards in pp.get_training_set.
#'
#' @param base_df long data frame containing all potential data 
#' @param modeling_options list of modeling options specified
#'
#' @return same as base_df but with additional lagged columns
#' @importFrom purrr map
#' @importFrom stringr str_pad
#' @importFrom forecast ets
#' @importFrom zoo na.trim
pp.add_lagged_columns <- function(base_df,
                                  modeling_options) {
  
  # determine largest lag to create
  max_lag <-
    max(unlist(sapply(modeling_options$base_covariates, function(x)
      x$lags))) + (modeling_options$roll_lags - 1)
  all_lags <- 1:max_lag

  lag_functions <- all_lags %>%
    purrr::map(function(x)
      ~ lag(., n = x, order_by = time_value))
  names(lag_functions) <- paste(LAG_SUFFIX, stringr::str_pad(all_lags, 2, pad="0"), sep = "_")

  # the latest reference date is the forecast date 
  max_ref_date <- modeling_options$forecast_date
  
  if (modeling_options$impute_last_3_response_covariate) {
    # impute the last 3 reference dates, as on a typical forecast date,
    # the data is only available with a 3 day lag.
    # imputation is done using an ets model
    ref_date_lag_3 <- modeling_options$forecast_date - 3

    last_3_imputed_df <- base_df %>%
      filter(variable_name == modeling_options$response) %>%
      group_by(geo_value, variable_name) %>%
      arrange(time_value) %>%
      tidyr::complete(
        time_value = seq.Date(min(time_value), max_ref_date, by = "day"),
        fill = list(value = NA)
      ) %>%
      mutate(value = ifelse(time_value > ref_date_lag_3,
                            c(rep(NA, n() - 3),
                              as.numeric(
                                predict(forecast::ets(
                                  ts(zoo::na.trim(value), frequency = 7),
                                ), h = 3)$mean
                              )),
                            value)) %>%
      ungroup

    lagged_df <- bind_rows(base_df %>% filter(variable_name != modeling_options$response),
                           last_3_imputed_df) %>%
      group_by(geo_value, variable_name) %>%
      arrange(time_value) %>%
      tidyr::complete(
        time_value = seq.Date(min(time_value), max_ref_date, by = "day"),
        fill = list(value = NA)
      ) %>%
      mutate_at(vars(value), .funs = lag_functions) %>%
      ungroup

  } else {
    lagged_df <- base_df %>%
      group_by(geo_value, variable_name) %>%
      arrange(time_value) %>%
      tidyr::complete(
        time_value = seq.Date(min(time_value), max_ref_date, by = "day"),
        fill = list(value = NA)
      ) %>%
      mutate_at(vars(value), .funs = lag_functions) %>%
      ungroup
  }

  lagged_df
}


#' Add principal components to location_info_df
#'
#' @param location_info_df the data frame
#' @param modeling_options the modeling options list
#'
#' @return tibble with columns location, population, PC01, ..., PC0n
#'
#' @importFrom stringr str_pad
pp.add_pc <- function(location_info_df,
                      modeling_options) {
  if (modeling_options$location_pcs > 0) {
    matx <- location_info_df %>% select(-geo_value) %>% as.matrix
    matx <- tr.column_impute(matx)
    matx <- scale(matx, TRUE, TRUE)
    n_pcs <- modeling_options$location_pcs
    if (n_pcs > ncol(matx)) {
      warning('Requested PCs (', n_pcs, ') exceeds number of columns (',
              ncol(matx), ').  Using fully reconstructed matrix.')
      n_pcs <- ncol(matx)
    }
    if (modeling_options$geo_type == "county") {
      decomp <- stats::princomp(matx)$loadings[, 1:n_pcs]
    } else {
      decomp <- svd(matx)$v[, 1:n_pcs]
    }
    top_pcs <- matx %*% decomp
    colnames(top_pcs) <-
      paste0('PC', stringr::str_pad(1:n_pcs, 2, pad = '0'))
    location_with_pcs <- bind_cols(location_info_df %>%
                                     select(geo_value, population),
                                   data.frame(top_pcs))
    return(location_with_pcs)
  } else {
    return(location_info_df %>%
             select(geo_value, population))
  }
}


#' Make predictors X and target y for a single forecast_date,
#' intermediate fitting function that returns all lagged covariates
#'
#' @param lagged_df original base_df (tall df) with lagged columns
#' @param location_info_df formerly health_rankings_df
#' @param modeling_options the modeling options list
#' @param forecast_date the forecast date
#'
#' @return list with names X, y, row_locations.  y is NA if forecast_date is in the future
#' @importFrom evalcast get_target_period
#' @importFrom lubridate ymd
pp._get_training_set <- function(lagged_df,
                                location_info_df,
                                modeling_options,
                                forecast_date) { 
  # get response (y)
  target_period_df <-
    evalcast::get_target_period(forecast_date,
                                modeling_options$incidence_period,
                                modeling_options$ahead)

  y <- lagged_df %>%
    filter(
      variable_name == modeling_options$response,
      time_value >= target_period_df$start &
        time_value <= target_period_df$end
    ) %>%
    group_by(geo_value, variable_name) %>%
    mutate(value = ifelse(is.na(value), mean(value, na.rm = TRUE), value)) %>% # impute NAs with group mean
    summarize(value = sum(value)) %>% ungroup %>%
    tidyr::pivot_wider(names_from = "variable_name", values_from = "value")

  # get original covariates (X)
  X <- lagged_df %>%
    filter(time_value == forecast_date) %>%
    tidyr::pivot_longer(
      -c(geo_value, variable_name, time_value),
      names_to = "lag_names",
      values_to = "value"
    ) %>%
    mutate(variable_name = ifelse(
      lag_names == "value",
      variable_name,
      paste0(variable_name, "_", lag_names)
    )) %>%
    select(-lag_names) %>%
    tidyr::pivot_wider(geo_value, names_from = "variable_name", values_from = "value")

  # add in location variables
  X <- X %>%
    left_join(location_info_df, by = "geo_value")

  # ensure rows in X and y correspond to the same location
  all_locations <- tibble(geo_value = unique(lagged_df$geo_value))
  if (forecast_date == modeling_options$forecast_date) {
    # Evaluation y
    y <- NULL
  } else {
    # Training y
    y <- all_locations %>% left_join(y, by = "geo_value") %>% select(modeling_options$response)
  }
  X <- all_locations %>% left_join(X, by = "geo_value") %>% select(-geo_value)

  # dfs: list of X, y, location map for the rows
  list(X = X,
       y = y,
       row_locations = all_locations)
}


#' Mean impute and select relevant variables
#'
#' @param train_test list of training and testing data
#' @param modeling_options list of modeling options
#'
#' @return list with names X, y, row_locations. y is NA if forecast_date is in the future
pp.impute_and_select <- function(train_test,
                                 modeling_options) {

  # check and drop all NA columns
  all_na_cols <- which(sapply(train_test$X, function(x) all(is.na(x))))
  if (length(all_na_cols) > 0) {
    train_test$X <- train_test$X[, -all_na_cols]
  }

  # perform mean imputation
  X <- tr.column_impute(as.matrix(train_test$X))

  # create rolling sums
  finished_vars <- c()
  for (base_var in c(modeling_options$base_covariates,
                     modeling_options$location_covariates)) {
    name <- base_var$name
    if (!(name %in% finished_vars) & (base_var$do_rollsum | base_var$do_rollavg)) {
      n_lags <- modeling_options$roll_lags
      lag_idx <- which(grepl(name, colnames(X)))
      n_total_lags <- length(lag_idx)
      if (n_total_lags < n_lags)
        next

      # create all 7 day lags
      lagged_var <- NULL
      if (base_var$do_rollsum) {
        # do roll summing
        for (i in seq(1, (n_total_lags - (n_lags - 1)))) {
          lagged_var <- cbind(lagged_var, rowSums(X[, lag_idx[i]:(lag_idx[i+(n_lags - 1)])]))
        }
        colnames(lagged_var) <- paste0(colnames(X)[lag_idx], "_", ROLLSUM_SUFFIX)[1:(n_total_lags - n_lags + 1)]

      } else {
        # do roll averaging 
        for (i in seq(1, (n_total_lags - (n_lags - 1)))) {
          lagged_var <- cbind(lagged_var, rowMeans(X[, lag_idx[i]:(lag_idx[i+(n_lags - 1)])]))
        }
        colnames(lagged_var) <- paste0(colnames(X)[lag_idx], "_", ROLLAVG_SUFFIX)[1:(n_total_lags - n_lags + 1)]
      }

      X <- cbind(X[, -lag_idx], data.frame(lagged_var, check.names = FALSE))
      finished_vars <- c(finished_vars, name)
    }
  }

  # select relevant variables to keep (could be shortened)
  keep_vars <- c()
  for (base_var in c(modeling_options$base_covariates,
                     modeling_options$location_covariates)) {
    name <- base_var$name
    for (lag_num in base_var$lags) {
      if (lag_num > 0) {
        if (base_var$do_rollsum) {
          keep_vars <- c(keep_vars, paste(name, LAG_SUFFIX,
                                          stringr::str_pad(lag_num, 2, pad="0"),
                                          ROLLSUM_SUFFIX, sep = "_"))
        } else if (base_var$do_rollavg) {
          keep_vars <- c(keep_vars, paste(name, LAG_SUFFIX,
                                          stringr::str_pad(lag_num, 2, pad="0"),
                                          ROLLAVG_SUFFIX, sep = "_"))
        } else {
          keep_vars <- c(keep_vars, paste(name, LAG_SUFFIX,
                                          stringr::str_pad(lag_num, 2, pad="0"),
                                          sep = "_"))
        }
      } else {
        if (base_var$do_rollsum) {
          keep_vars <- c(keep_vars, paste(name, ROLLSUM_SUFFIX, sep = "_"))
        } else if (base_var$do_rollavg) {
          keep_vars <- c(keep_vars, paste(name, ROLLAVG_SUFFIX, sep = "_"))
        } else {
          keep_vars <- c(keep_vars, name)
        }
      }
    }
  }
  keep_vars <- c(keep_vars, paste0('PC', stringr::str_pad(1:modeling_options$location_pcs, 2, pad='0')))
  train_test$X <- X[, which(colnames(X) %in% keep_vars)]

  train_test
}

#' Make predictors X and target y for a single forecast_date
#'
#' @param lagged_df original base_df (tall_df) with lagged columns
#' @param location_info_df formerly health_rankings_df
#' @param modeling_options the modeling options list
#' @param forecast_date the forecast date
#'
#' @return list with names X, y, row_locations.  y is NA if forecast_date is in the future
#' @importFrom evalcast get_target_period
#' @importFrom lubridate ymd
pp.get_training_set <- function(lagged_df,
                                location_info_df,
                                modeling_options,
                                forecast_date) {

  intermediate_train_test <- pp._get_training_set(lagged_df,
                                                  location_info_df,
                                                  modeling_options,
                                                  forecast_date)

  pp.impute_and_select(intermediate_train_test,
                       modeling_options)
}

#' Make predictors X and target y for stacked forecast dates.
#'
#' @param lagged_df original base_df (tall_df) with lagged columns
#' @param location_info_df formerly health_rankings_df
#' @param modeling_options todo
#' @param forecast_date date on which we start producing forecasts
#'
#' @return list with names X, y, row_location.  y is NA if forecast_date is in the future
#'
#' @importFrom logger log_debug log_info
pp.get_stacked_training_set <-
  function(lagged_df,
           location_info_df,
           modeling_options,
           forecast_date) {
    X <- vector("list", modeling_options$weeks_back)
    y <- vector("list", modeling_options$weeks_back)
    row_locations <- vector("list", modeling_options$weeks_back)
    for (idx in 1:(modeling_options$weeks_back)) {
      forecast_date_k_back <-
        forecast_date - 7 * (modeling_options$ahead + idx - 1)
      logger::log_info(paste0("Getting training set for ", forecast_date_k_back))
      training_set <- pp._get_training_set(lagged_df,
                                           location_info_df,
                                           modeling_options,
                                           forecast_date_k_back)
      X[[idx]] <- training_set$X
      y[[idx]] <- training_set$y
      row_locations[[idx]] <- training_set$row_locations
    }

    stacked_X <- bind_rows(X)
    stacked_y <- bind_rows(y)
    stacked_row_locations <- bind_rows(row_locations)


    intermediate_train_test <- list(X = stacked_X,
                                    y = stacked_y,
                                    row_locations = stacked_row_locations)

    pp.impute_and_select(intermediate_train_test,
                         modeling_options)
  }


#' Perform additional preprocessing steps
#'
#' @param train_test list of training and testing data
#' @param modeling_options a named list, additional elements of which
#'     overrides learner-dependent options set within the code
#'     
#' @return same as train_test, but with additional preprocessing sets and
#'         validation splits
#' @importFrom logger log_debug log_info
#' @importFrom stringr str_detect
pp.transform_and_scale <- function(train_test,
                                   modeling_options) {
  # check for columns all NAs
  if (sum(sapply(train_test$train_y, function(x) all(is.na(x)))) > 0)
    stop("response is all NA")
  all_na_train <- sapply(train_test$train_X, function(x) all(is.na(x)))
  all_na_test <- sapply(train_test$test_X, function(x) all(is.na(x)))
  all_na_both <- which(all_na_train | all_na_test)
  if (length(all_na_both) > 0) {
    logger::log_info(paste0("dropping all NA column(s): ", names(all_na_both)))
    train_test$train_X <- train_test$train_X[, -all_na_both]
    train_test$test_X <- train_test$test_X[, -all_na_both]
  }

  logger::log_info("Final set of covariates:\n",
                   paste(colnames(train_test$train_X),
                         collapse='\n'))

  # imputation
  train_test$train_X <- tr.column_impute(as.matrix(train_test$train_X))
  train_test$test_X <- tr.column_impute(as.matrix(train_test$test_X))

  # transform response
  if (modeling_options$log_response) {
    train_test$train_y <- tr.log_pad(train_test$train_y)
  }

  # transform base covariates
  present_vars <- colnames(train_test$train_X)
  new_train_X <- train_test$train_X
  new_test_X <- train_test$test_X
  all_specified_base_vars <-
    sapply(modeling_options$base_covariates, function(x)
      x$name)
  for (i in 1:ncol(new_train_X)) {
    var_opt_idx <- which(stringr::str_detect(present_vars[i], all_specified_base_vars))
    if (length(var_opt_idx) > 0) {
      var_opt <- modeling_options$base_covariates[[var_opt_idx[1]]]
      if (length(var_opt_idx) > 1) {
        logger::log_debug(paste(var_opt$name,
                                "specified more than once, transforming",
                     "with function specified in model_covariates"))
      }
      new_train_X[,i] <- var_opt$tr(train_test$train_X[,i])
      new_test_X[,i] <- var_opt$tr(train_test$test_X[,i])
    }
  }

  # transform location covariates
  all_specified_loc_vars <-
    sapply(modeling_options$location_covariates, function(x)
      x$name)
  for (i in 1:ncol(new_train_X)) {
    var_opt_idx <- which(stringr::str_detect(present_vars[i], all_specified_loc_vars))
    if (length(var_opt_idx) > 0) {
      var_opt <- modeling_options$location_covariates[[var_opt_idx[1]]]
      new_train_X[,i] <- var_opt$tr(train_test$train_X[,i])
      new_test_X[,i] <- var_opt$tr(train_test$test_X[,i])
    }
  }

  # standardization - we scale the columns to have norm equal to the
  # norm of first lagged response
  ## note there is no solution if no lagged response!
  # if we have the strawman, omit the strawman from this scaling
  # if we have dummies, omit the dummies from this scaling
  scale_to_first <- function(sub_X,
                             modeling_options) {
    omit_vars <- stringr::str_detect(colnames(sub_X),
                                     modeling_options$response)
    first_lagged <- which(omit_vars)[1]
    std_dev <- apply(sub_X, 2, sd)
    sub_X[, !omit_vars] <-
      scale(sub_X[, !omit_vars], TRUE, std_dev[!omit_vars])

    sub_X[,!omit_vars][, std_dev[!omit_vars] == 0] <- 0
    norm_of_lagged <- sd(sub_X[,first_lagged])
    sub_X[,!omit_vars] <-
      sub_X[,!omit_vars] * norm_of_lagged
    sub_X
  }

  # split into train-valid-test sets (but retain original set for final global training)
  n_fit <- floor(0.5 * nrow(new_train_X)) + 1
  fit_inds <- c(n_fit:nrow(new_train_X)) #sample(1:nrow(new_train_X), n_fit, replace = FALSE)

  fit_train_X <- new_train_X[fit_inds, ]
  fit_train_y <- train_test$train_y[fit_inds, ]
  validate_train_X <- new_train_X[-fit_inds, ]
  validate_train_y <- train_test$train_y[-fit_inds, ]
  train_test$fit_train_row_locations <- train_test$train_row_locations[fit_inds, ]
  train_test$validate_train_row_locations <- train_test$train_row_locations[-fit_inds, ]
  train_test$fit_train_y <- fit_train_y
  train_test$validate_train_y <- validate_train_y

  train_test$train_X <- scale_to_first(new_train_X, modeling_options)
  train_test$test_X <- scale_to_first(new_test_X, modeling_options)
  train_test$fit_train_X <- train_test$train_X[fit_inds, ]
  train_test$validate_train_X <- scale_to_first(validate_train_X, modeling_options)

  if (!is.null(modeling_options$debug_folder)) {
    dir.create(file.path(modeling_options$debug_folder))
    out_file <- file.path(
      modeling_options$debug_folder,
      paste0("debug_",
             modeling_options$forecast_date,  "_ahead_",
             modeling_options$ahead, ".Rdata")
    )
    save(train_test,modeling_options,
         file = out_file)
    logger::log_debug(paste0("Saved ", out_file))
  }

  train_test
}


#' Make train predictors, targets and test predictors
#'
#' @param base_df the data frame
#' @param location_info_df formerly health_rankings_df
#' @param forecast_date date on which we start producing forecasts
#' @param modeling_options the modeling options list
#'
#' @return list with names train_X, train_y, test_X
#' @importFrom logger log_debug log_info
pp.make_train_test <- function(base_df,
                               location_info_df,
                               forecast_date,
                               modeling_options) {
  # filter to available data as of forecast_date
  filtered_df <- base_df %>% filter(time_value <= forecast_date)

  # filter to variables of interest
  vars_to_keep <-
    sapply(modeling_options$base_covariates, function(x)
      x$name)
  filtered_df <- base_df %>%
    filter(variable_name %in% c(modeling_options$response, vars_to_keep))

  ## At this point, assume each reference date only appears once per date,
  ## location, variable_name
  lagged_df <- pp.add_lagged_columns(filtered_df, modeling_options)

  # filter out state level FIPS codes
  if (modeling_options$geo_type == "county") {
    lagged_df <- lagged_df %>% filter(substr(.data$geo_value, 3, 5) != "000")
  }

  location_info_df <- pp.add_pc(location_info_df, modeling_options)
  test_dfs <- pp.get_training_set(lagged_df,
                                  location_info_df,
                                  modeling_options,
                                  forecast_date)
  train_dfs <- pp.get_stacked_training_set(lagged_df,
                                           location_info_df,
                                           modeling_options,
                                           forecast_date)

  ## take intersection of covariates in test/train
  ## (some are not available at earlier/later dates)
  all_vars <- lubridate::union(colnames(train_dfs$X), colnames(test_dfs$X))
  common_vars <- lubridate::intersect(colnames(train_dfs$X), colnames(test_dfs$X))
  if (length(common_vars) < length(all_vars)) {
    train_dfs$X <- train_dfs$X[, common_vars]
    test_dfs$X <- test_dfs$X[, common_vars]
    logger::log_info(paste0("pp.make_train_test: dropping non-intersected variables ",
                     lubridate::setdiff(all_vars, common_vars), collapse='\n'))
  }

  return(pp.transform_and_scale(list(
    train_X = train_dfs$X,
    train_y = train_dfs$y,
    train_row_locations = train_dfs$row_locations,
    test_X = test_dfs$X,
    test_row_locations = test_dfs$row_locations
  ), modeling_options))
}
