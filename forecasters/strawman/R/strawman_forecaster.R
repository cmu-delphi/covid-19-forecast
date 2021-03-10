
strawman_forecaster <- function(version = 2, ahead = 1,
                                response,
                                incidence_period
                                ) {

    ## response <- "hospitalizations"  # used for removing untrustworthy entries

    ## After how many days do you trust the value?  Note, if you do not trust
    ## the value, we simply throw it out right now.  We do not have a better way
    ## of handling this yet.
    ## backfill_buffer <- 5

    function(df, forecast_date) {
      strawman_forecaster_raw(df = df,
                              forecast_date = forecast_date,
                              response = response,
                              version = version,
                              ahead = ahead,
                              incidence_period = incidence_period)
    }
}


#' @importFrom MMWRweek MMWRweek
strawman_forecaster_raw <- function(df,
                                    ahead,
                                    incidence_period,
                                    response,
                                    forecast_date,
                                    version) {


  # Every forecaster should start with this.
  stopifnot(c("location", "reference_date", "issue_date") %in% names(df))
  min_issue_date <- min(df$issue_date, na.rm = TRUE)
  df_train <- df %>%
    filter(
      (is.na(.data$issue_date) & (.data$reference_date <= forecast_date)) |
        .data$issue_date <= forecast_date
    )
  df <- tibble() # make it empty to ensure it isn't accidentally used.





  ## ## We need these variables in order to do forecasting.
  ## incidence_period <- "epiweek"

  ## response <- "usafacts_deaths_incidence_num"
  ## # response <- "hospitalizations"  # used for removing untrustworthy entries

  ## ## After how many days do you trust the value?  Note, if you do not trust
  ## ## the value, we simply throw it out right now.  We do not have a better way
  ## ## of handling this yet.
  ## # backfill_buffer <- 5


  ## # The below is the only temporal function we will use.
  ##   ##require(MMWRweek)

  target_period <- evalcast::get_target_period(
    forecast_date, incidence_period, ahead)

  target_dates <- seq(target_period$start, target_period$end, by = "day")

  target_epiweeks <- MMWRweek(target_dates) %>%
    select(.data$MMWRyear, .data$MMWRweek) %>%
    distinct()

  target_epiweek <- target_epiweeks %>% 
    transmute(epiweek = 100 * .data$MMWRyear + .data$MMWRweek) %>% 
    pull(.data$epiweek)

  ######################################################################

  min_cutoff_date <- "2020-03-15" # We don't want to use data before this

  df_train <- filter(df_train, .data$variable_name == response)

  # Not expecting issue dates yet
  stopifnot(all(is.na(df_train$issue_date)))


  df_train <- filter(df_train, .data$reference_date >= min_cutoff_date)

  . <- "define this to kill NOTEs"
  df_train <- df_train %>%
    select(-.data$location_name) %>%
    mutate(issue_date = coalesce(.data$issue_date, .data$reference_date - 1)) %>%
    group_by(.data$location, .data$reference_date, .data$variable_name) %>%
    slice(which.max(.data$issue_date)) %>%
    ungroup() %>%
    select(-.data$issue_date) %>%
    {.}

  df_train <- zookeeper::multinomial_preprocessor(
    df_train, response = response, max_lag = 5)



  ## We only use deaths, this is the strawman
  stopifnot(length(unique(df_train$variable_name)) == 1)
  df_train <- select(df_train, -.data$variable_name)

  epiweeks <- MMWRweek(df_train$reference_date)

  if(any(epiweeks$MMWRyear > 2020)) {
    warning("epiyear > 2020; Check if differences are done correctly") # RS
  }

  df_train <- mutate(df_train,
                     epiweek = 100 * epiweeks$MMWRyear + epiweeks$MMWRweek)


  ## This is to ensure we did not have multiple entries for a 
  ## location on a reference date
  check_nrow <- nrow(df_train)
  df_train <- distinct(df_train, .data$location, .data$reference_date, 
                       .keep_all = TRUE)
  stopifnot(check_nrow == nrow(df_train))


  df_epiweek <- df_train %>%
    group_by(.data$location, .data$epiweek) %>%
    summarise(value = sum(.data$value)) %>%
    ungroup()

  if(version == 1){
    lag_length <- ahead
  } else if(version == 2){
    lag_length <- 1
  } else{
    stop("Invalid version.")
  }


  # This assumes that there is no missing epiweek for any county.

  df_epiweek <- df_epiweek %>%
    group_by(.data$location) %>%
    mutate(
      increase = .data$value - dplyr::lag(
        .data$value, n = lag_length, default = 0, order_by = .data$epiweek)) %>%
    ungroup()


  covidhub_probs <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)

  output <- strawman_quantiles_all_regions(df_epiweek,
                                           target_epiweek = target_epiweek,
                                           probs = covidhub_probs,
                                           version = version,
                                           ahead = ahead)

  return(output)
}
