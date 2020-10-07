
strawman_forecaster <- function(version = 2, ahead = 1,
                                response,
                                incidence_period
                                ) {

    ## response <- "hospitalizations"  # used for removing untrustworthy entries

    ## After how many days do you trust the value?  Note, if you do not trust
    ## the value, we simply throw it out right now.  We do not have a better way
    ## of handling this yet.
    ## backfill_buffer <- 5

    function(df, forecast_date) strawman_forecaster_raw(df = df,
                                                        forecast_date = forecast_date,
                                                        response = response,
                                                        version = version,
                                                        ahead = ahead,
                                                        incidence_period = incidence_period)
}

#' @importFrom evalforecast multinomial_preprocesser
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
    filter((is.na(issue_date) &
              (reference_date <= forecast_date)) | issue_date <= forecast_date)
  df <-
    tibble() # make it empty to ensure it isn't accidentally used.





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

  target_period <- evalforecast::get_target_period(forecast_date,
                                                   incidence_period, ahead)

  target_dates <- seq(target_period$start, target_period$end, by = "day")

  target_epiweeks <- MMWRweek(target_dates) %>%
    select(MMWRyear, MMWRweek) %>%
    distinct()

  target_epiweek <- target_epiweeks %>% 
    transmute(epiweek = 100 * MMWRyear + MMWRweek) %>% pull(epiweek)

  ######################################################################

  min_cutoff_date <- "2020-03-15" # We don't want to use data before this

  df_train <- filter(df_train, variable_name == response)

  # Not expecting issue dates yet
  stopifnot(all(is.na(df_train$issue_date)))


  df_train <- filter(df_train, reference_date >= min_cutoff_date)

  df_train <- df_train %>%
    select(-location_name) %>%
    mutate(issue_date = coalesce(issue_date, reference_date - 1)) %>%
    group_by(location, reference_date, variable_name) %>%
    slice(which.max(issue_date)) %>%
    ungroup() %>%
    select(-issue_date) %>%
    {.}

  df_train <- multinomial_preprocesser(df_train,
                                       response = response,
                                       max_lag = 5)



  ## We only use deaths, this is the strawman
  stopifnot(length(unique(df_train$variable_name)) == 1)
  df_train <- select(df_train, -variable_name)

  epiweeks <- MMWRweek(df_train$reference_date)

  if(any(epiweeks$MMWRyear > 2020)) warning("epiyear > 2020; Check if differences are done correctly") # RS

  df_train <- mutate(df_train,
                     epiweek = 100 * epiweeks$MMWRyear + epiweeks$MMWRweek)


  ## This is to ensure we did not have multiple entries for a location on a reference date
  check_nrow <- nrow(df_train)
  df_train <- distinct(df_train, location, reference_date, .keep_all = TRUE)
  stopifnot(check_nrow == nrow(df_train))


  df_epiweek <- df_train %>%
    group_by(location, epiweek) %>%
    summarise(value = sum(value)) %>%
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
    group_by(location) %>%
    mutate(
      increase = value - dplyr::lag(
        value, n = lag_length, default = 0, order_by = epiweek)
      ) %>%
    ungroup()


  cdc_probs <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)

  output <- strawman_quantiles_all_regions(df_epiweek,
                                           target_epiweek = target_epiweek,
                                           probs = cdc_probs,
                                           version = version,
                                           ahead = ahead)

  return(output)

}
