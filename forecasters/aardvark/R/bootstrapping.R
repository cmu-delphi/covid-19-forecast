make_gaussian_bootstrap_by_geo_value <- function(ave = weighted.mean, bandwidth = 14){

  gaussian_bootstrap_by_geo_value <- function(df_point_preds, forecast_date,
                                              incidence_period, ahead, B = 1000){

    target_dates <- get_target_period(forecast_date, incidence_period, ahead) %$%
      seq(start,end,by = "days")
    point_preds <- df_point_preds %>% 
      filter(time_value %in% target_dates) %>%
      select(geo_value, time_value, preds)
    stopifnot(nrow(point_preds) == length(unique(point_preds$geo_value)) * length(target_dates))
    point_preds <- point_preds %>% 
      group_by(geo_value) %>%
      summarize(preds = sum(preds), .groups = "drop") %>%
      mutate(time_value = NA)

    df_resids <-  df_point_preds %>% 
      select(geo_value, time_value, observed_value, preds) %>%
      filter(!is.na(observed_value)) %>%
      mutate(resids = observed_value - preds,
             weights = tricube( as.numeric(forecast_date - time_value) / bandwidth)) %>%
      select(-c(observed_value, preds)) %>% 
      group_by(geo_value) %>%
      arrange(time_value) %>%
      mutate(resids = rollsum(resids, 7, fill = NA, align = "right"))

    df_vars_empty <- data.frame(geo_value = unique(df_point_preds$geo_value))
    df_vars <- df_resids %>% 
      group_by(geo_value) %>%
      summarize(scale = sqrt(ave(resids ^ 2, w = weights, na.rm = T)), .groups = "drop")
    df_vars <- left_join(df_vars_empty, df_vars, by = "geo_value")

    stopifnot(nrow(point_preds) == nrow(df_vars))
    df_distribution <- left_join(point_preds, df_vars, by = "geo_value")
    df_distribution <- df_distribution %>% mutate(scale = if_else(is.na(scale), abs(preds), scale))
    preds_vec <- df_distribution$preds
    sds_vec <- df_distribution$scale
    replicates <- as.data.frame(matrix(rnorm(B * length(preds_vec), mean = preds_vec, sd = sds_vec),
                                       ncol = B, nrow = length(preds_vec)))
    colnames(replicates) <- paste0("replicate_", 1:B)

    bootstrap_preds <- bind_cols(df_distribution, replicates) %>%
      select(geo_value, time_value, starts_with("replicate_"))
    return(bootstrap_preds)
  }
}

tricube <- function(u){
  pmax(70/81 * (1 - u^3)^3, 0)
}
