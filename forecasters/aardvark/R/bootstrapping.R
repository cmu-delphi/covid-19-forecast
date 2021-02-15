make_by_location_gaussian_bootstrap_weekly <- function(ave, bandwidth){
  # Input:
  # -- ave: a function which takes as inputs x and w, and computes a weighted average.
  # -- bandwidth: determines the weights for the weighted average function.

  by_location_gaussian_bootstrap_weekly <- function(B, df_point_preds, forecast_date, incidence_period, ahead){
    # First, compute one std. dev. estimate per location, by taking weighted average of squared residuals.
    # Second, resample squared residuals using a separate Gaussian bootstrap *for each location*.
    # Third, back out Monte Carlo samples for Y_{t,\ell}.

    # (1) Get conditional mean predictions, on weekly scale.
    target_dates <- get_target_period(forecast_date, incidence_period, ahead) %$%
      seq(start,end,by = "days")
    point_preds <- df_point_preds %>% 
      filter(time_value %in% target_dates) %>%
      select(location, time_value, preds)
    stopifnot(nrow(point_preds) == length(unique(point_preds$location)) * length(target_dates))
    point_preds <- point_preds %>% 
      group_by(location) %>%
      summarize(preds = sum(preds), .groups = "drop") %>%
      mutate(time_value = NA)

    # (2) Get conditional standard deviation estimates, on weekly scale
    df_resids <-  df_point_preds %>% 
      select(location, time_value, observed_value, preds) %>%
      filter(!is.na(observed_value)) %>%
      mutate(resids = observed_value - preds,
             weights = tricube( as.numeric(forecast_date - time_value) / bandwidth)) %>%
      select(-c(observed_value, preds)) %>% 
      group_by(location) %>%
      arrange(time_value) %>%
      mutate(resids = rollsum(resids, 7, fill = NA, align = "right"))

    df_vars_empty <- data.frame(location = unique(df_point_preds$location))
    df_vars <- df_resids %>% 
      group_by(location) %>%
      summarize(scale = sqrt(ave(resids ^ 2, w = weights, na.rm = T)), .groups = "drop")
    df_vars <- left_join(df_vars_empty, df_vars, by = "location")

    # (3) Combine conditional mean and conditional standard deviation estimates
    stopifnot(nrow(point_preds) == nrow(df_vars))
    df_distribution <- left_join(point_preds, df_vars, by = "location")
    cat(paste0("A total of ", sum(is.na(df_vars$scale)), " locations have no validation set.\n"))
    
    # Not really sure what the thinking here was
    df_distribution <- df_distribution %>% mutate(scale = if_else(is.na(scale), abs(preds), scale))

    # (4) Draw replicates from a Gaussian distribution with given scale and mean.
    preds_vec <- df_distribution$preds
    sds_vec <- df_distribution$scale
    replicates <- as.data.frame(matrix(rnorm(B * length(preds_vec), mean = preds_vec, sd = sds_vec),
                                       ncol = B, nrow = length(preds_vec)))
    colnames(replicates) <- paste0("replicate_", 1:B)

    bootstrap_preds <- bind_cols(df_distribution, replicates) %>%
      select(location, time_value, starts_with("replicate_"))
    return(bootstrap_preds)
  }
}

tricube <- function(u){
  pmax(70/81 * (1 - u^3)^3, 0)
}

#wild.sampler <- function(data){
#  wild.boot.resids <- data$tf.residuals * sample(x = c((1+sqrt(5))/2, 1-sqrt(5)/2), 
#                                                 size = nrow(data), 
#                                                 replace = TRUE, 
#                                                 prob = c((1+sqrt(5))/(2*sqrt(5)),
#                                                          (sqrt(5)-1)/(2*sqrt(5))))
#  wild.boot.sample <- data$tf.estimate + wild.boot.resids
#  return(data.frame(x=data$x,y=wild.boot.sample,wts=data$wts))
#}

#data$tf.estimate <- tf.estimator(data, lambda.opt, k, x.eval.grid)
#data$tf.residuals <- data$y - data$tf.estimate

#tf.boot.ensemble <- matrix(unlist(replicate(B,tf.estimator(wild.sampler(data), lambda.opt, k,
#                                                           x.eval.grid, max_iter = max_iter, obj_tol = obj_tol))),
#                           ncol = B)

#bootstrap.lower.perc.intervals <- apply(tf.boot.ensemble,1,quantile,probs = alpha/2)
#bootstrap.upper.perc.intervals <- apply(tf.boot.ensemble,1,quantile,probs = 1-alpha/2)
