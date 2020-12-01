make_mean_imputer <- function(k, align = "right", first_date = NULL, last_date = NULL, ave = rollmean){
  # Closure to make an imputation function.
  # Inputs:
  # -- k: number of days to take an mean over
  # -- align: one of "left", "right" or "center". See zoo::rollmean for details.
  # -- first_date: Date object, the first date on which we should see variables.
  # -- last_date: Date object, the last date on which we should see variables.

  mean_imputer <- function(dat){
    # For each date and location, replace value by a mean over the k nearest days.
    # Input:
    # -- dat: data frame with columns "location", "date", and "value" 
    #         (and possibly others, which we will ignore)

    stopifnot( c("location", "date", "value") %in% names(dat) )

    if ( is.null(first_date) ){
      first_date <- min(dat %>% pull(date))
    }
    if ( is.null(last_date) ){
      last_date <- max(dat %>% pull(date))
    }
    date_df <- data.frame(date = seq(first_date, last_date, by = "days"))
    full_df <- left_join(date_df, dat, by = c("date"))

    imputed_dat <- full_df %>%
      group_by(location) %>%
      arrange(date) %>%
      mutate(imputed_value = ave(value, k, align = align, fill = "extend")) %>%
      ungroup
    return(imputed_dat)
  }
}