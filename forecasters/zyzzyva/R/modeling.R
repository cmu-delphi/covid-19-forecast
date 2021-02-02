#' @include calibration.R transformation.R
NULL

#' Fit a learner to the training data
#'
#' @param train_test list of training and testing data
#' @param modeling_options a named list, additional elements of which
#'     overrides learner-dependent options set within the code
ml.fit_model <- function(train_test,
                         modeling_options) {
  if (modeling_options$learner == "stratified_linear") {
    ml.stratified_linear(train_test, modeling_options)
  } else {
    stop("available learners are ('stratified_linear')")
  }
}

#' Finds the stratified clusters. 
#'
#' @param test_X covariates as of forecast_date
#' @param modeling_options a named list, additional elements of which
#'     overrides learner-dependent options set within the code
#' @importFrom stats quantile
ml.get_clusters <- function(test_X,
                            modeling_options) {

  # get variable names for clustering
  cluster_vars <- c()
  for (var in modeling_options$cluster_covariates) {
    name <- var$name
    for (lag_num in var$lags) {
      if (lag_num > 0) {
        temp_name <- paste(name, LAG_SUFFIX, 
        stringr::str_pad(lag_num, 2, pad="0"), sep="_")
        if (var$do_rollavg) {
          temp_name <- paste(temp_name, ROLLAVG_SUFFIX, sep="_")
        } else if (var$do_rollsum) {
          temp_name <- paste(temp_name, ROLLSUM_SUFFIX, sep="_")
        }
        cluster_vars <- c(cluster_vars, temp_name)
      } else {
        if (var$do_rollsum) {
          cluster_vars <- c(cluster_vars, paste(name, ROLLSUM_SUFFIX, sep = "_"))
        } else if (var$do_rollavg) {
          cluster_vars <- c(cluster_vars, paste(name, ROLLAVG_SUFFIX, sep = "_"))
        } else {
          cluster_vars <- c(cluster_vars, name)
        }
      }
    }
  }
    
  # perform simple equal-size clustering based off cases
  n_clusters <- modeling_options$n_clusters
  cluster_cols_idx <- which(colnames(test_X) %in% cluster_vars)
  cluster_vars <- test_X[, cluster_cols_idx]
  sum_cols <- rowSums(cluster_vars)
  clusters <- rep(1, length(sum_cols))
  cluster_quantiles <- stats::quantile(sum_cols, c(1:(n_clusters - 1)) / n_clusters)
  for (i in 1:(n_clusters - 1)) {
    clusters[sum_cols>cluster_quantiles[i]] <- i + 1
  }
  clusters
}

#' Fit a simple linear model within stratified clusters.
#'
#' Clusters are determined using a simple scheme: order the locations
#' by the number of cases. Sort, then divide into k equal sized groups. 
#' Within each cluster, the following model is fit:
#'    cases ~ cases_lag1 + fb_lag1 + combined_ind_lag1 + log(pop) +
#'            slope(cases) + slope(fb) + slope(combined_ind).
#' The slope is computed using least squares over past lags of each signal,
#' for each location individually.
#' Calibration is not currently performed.
#' 
#' @param train_test list of training and testing data
#' @param modeling_options a named list, additional elements of which
#'     overrides learner-dependent options set within the code
#' @importFrom logger log_debug
#' @importFrom Iso pava
#' @importFrom stats lsfit
#' @importFrom quantgen quantile_lasso
#' @importFrom glmnet glmnet
ml.stratified_linear <- function(train_test,
                                 modeling_options) {
  # extract items from train_test
  train_X <- train_test$train_X
  train_y <- train_test$train_y
  test_X <- train_test$test_X
  train_row_locations <- train_test$train_row_locations[[1]]
  test_row_locations <- train_test$test_row_locations[[1]]
  n <- nrow(train_X)
  
  # get clusters
  clusters <- ml.get_clusters(test_X, modeling_options)
  cluster_locs <- data.frame(cluster_id=clusters,
                             location=train_test$test_row_locations)
  
  # create new_train_X, new_test_X with slope variables
  slope_base_vars <- c("usa-facts_confirmed_incidence_num_lag",
                       "fb-survey_smoothed_hh_cmnty_cli_lag",
                       "indicator-combination_nmf_day_doc_fbc_fbs_ght_lag")
  new_train_X <- matrix(NA, nrow=nrow(train_X), ncol=length(slope_base_vars))
  new_test_X <-  matrix(NA, nrow=nrow(test_X), ncol=length(slope_base_vars))
  for (i in 1:length(slope_base_vars)) {
    var <- slope_base_vars[i]
    n_chars <- nchar(var)
    var_cols <- which(substring(colnames(train_X), 1, n_chars)==var)
    n_var_cols <- length(var_cols)
    if (n_var_cols <= 1) {
      stop("Could not create slope vars in training X, not enough columns for ", var)
    }
    new_train_X[,i] <- stats::lsfit(-(1:n_var_cols), t(train_X[, var_cols]))$coef[2,]
    new_test_X[,i] <- stats::lsfit(-(1:n_var_cols), t(test_X[, var_cols]))$coef[2,]
  }
  
  # add first lags of handpicked variables
  first_case_var <- which(grepl("usa-facts_confirmed_incidence_num_lag", colnames(train_X)))[1]
  first_fb_var <- which(grepl("fb-survey_smoothed_hh_cmnty_cli_lag", colnames(train_X)))[1]
  first_ind_var <- which(grepl("indicator-combination_nmf_day_doc_fbc_fbs_ght_lag", colnames(train_X)))[1]
  pop_var <- which(grepl("population", colnames(train_X)))[1]
  new_train_X <- cbind(new_train_X,
                       train_X[, first_case_var],
                       train_X[, first_fb_var],
                       train_X[, first_ind_var],
                       train_X[, pop_var])
  new_test_X <- cbind(new_test_X,
                      test_X[, first_case_var],
                      test_X[, first_fb_var],
                      test_X[, first_ind_var],
                      test_X[, pop_var])
  
  covariate_names <- c(paste(slope_base_vars, "slope", sep="_"),
                       colnames(train_X)[first_case_var],
                       colnames(train_X)[first_fb_var],
                       colnames(train_X)[first_ind_var],
                       colnames(train_X)[pop_var])
  
  colnames(new_train_X) <- covariate_names
  colnames(new_test_X) <- covariate_names
  logger::log_debug("Input colnames:\n",
                    paste(colnames(new_train_X), collapse='\n'))
  
  # finally, get predictions
  out <- matrix(NA, nrow=nrow(test_X), ncol=length(modeling_options$cdc_probs))
  quantile_fits <- list()
  ls_fits <- list()
  for (i in 1:modeling_options$n_clusters) {
    logger::log_debug(paste("Fitting cluster", i))
    locs_in_cluster <- cluster_locs %>% 
      filter(cluster_id == i) %>%
      pull(geo_value)
    train_idx <- which(train_row_locations %in% locs_in_cluster)
    test_idx <- which(test_row_locations %in% locs_in_cluster)
    
    # get quantiles
    cluster_fit_quantiles <- quantgen::quantile_lasso(new_train_X[train_idx,],
                                                      train_y[train_idx,],
                                                      tau = modeling_options$cdc_probs,
                                                      standardize = FALSE,
                                                      lambda = 0)
    preds <-  predict(cluster_fit_quantiles, new_test_X[test_idx, ], sort = TRUE)
    colnames(preds) <- modeling_options$cdc_probs
    
    # maybe replace center point estimate with LS fit
    cluster_fit_ls <- NULL
    if (!modeling_options$use_median_point) {
      logger::log_debug("Replacing median estimate with LS point estimate")
      cluster_fit_ls <- glmnet::glmnet(new_train_X[train_idx,],
                                    train_y[train_idx,],
                                    standardize=FALSE,
                                    alpha=0, # 0=ridge, 1=lasso
                                    lambda=0)
      
      preds[, colnames(preds) == "0.5"] <- predict(cluster_fit_ls, new_test_X[test_idx,])
    }
    
    # save fits for debug
    quantile_fits[[i]] <- cluster_fit_quantiles
    ls_fits[[i]] <- cluster_fit_ls
    
    out[test_idx, ] <- preds
  }

  # output debug objects
  if (!is.null(modeling_options$debug_clusters_folder)) {
    dir.create(file.path(modeling_options$debug_clusters_folder))
    out_file <- file.path(
      modeling_options$debug_clusters_folder,
      paste0("debug_", modeling_options$forecast_date,
             "_ahead_", modeling_options$ahead, ".RDS")
    )
    out_obj <- list(
        modeling_options = modeling_options,
        train_test = train_test,
        cluster_locs = cluster_locs,
        ls_fits = ls_fits,
        quantile_fits = quantile_fits,
        train_X = new_train_X,
        train_y = train_y,
        test_X = new_test_X
      )
    saveRDS(out_obj, out_file)
    logger::log_debug(paste0("Saved ", out_file))
  }
  
  # isotonic regression to ensure quantiles ordering
  for (i in 1:nrow(out)) {
    out[i, ] <- Iso::pava(out[i, ])
  }
  
  # transform back to correct scale for clipping
  if (modeling_options$log_response) {
    out <- tr.inv_log_pad(out)
  }
  
  # ensure negatives are set to 0
  out[out < 0] <- 0
  
  # format for output
  final_out <- NULL
  for (i in 1:length(modeling_options$cdc_probs)) {
    final_out <- rbind(final_out, cbind(modeling_options$ahead,
                                        train_test$test_row_locations, 
                                        modeling_options$cdc_probs[i],
                                        out[, i]))
  }
  names(final_out) <- c("ahead", "geo_value", "quantile", "value")
  
  final_out
}

