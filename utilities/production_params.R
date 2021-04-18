## Obtain environment variables

## One place for all parameters used by all production scripts (production_script.R) and
## and also markdowns (anteater.Rmd zebra.Rmd)

# Common parameters -------------------------------------------------------

forecast_date <- lubridate::ymd(Sys.getenv("FORECAST_DATE"))
today  <- lubridate::ymd(Sys.getenv("TODAY"))
##output_dir  <- Sys.getenv("OUTPUT_DIR")
## We can fix this at /mnt
output_dir  <- "/mnt"
aheads  <- 1:4
state_output_subdir  <- "state-output"
county_output_subdir <- "county-output"
qa_lookback <- 60 # how far back do we show actual data on the QA report?
correction_lookback <- 90 # how far back do we look on the daily corrections report?
n_counties <- 200 # we predict the top 200 counties

# Signals used by the various forecasters ---------------------------------

## Here is where you change the signals if one or the other is not available,
## or even add other replacement forecasters
forecaster_details  <- list(

  anteater = list(
    signals = dplyr::tibble( # state death forecaster
      data_source = "jhu-csse",
      signal = c("deaths_incidence_num",
                 "confirmed_incidence_num"
                 ),
      start_day = animalia::grab_start_day(aheads, 28, 14, "epiweek")(forecast_date),
      geo_type = "state"
    ),
    ## The corresponding QC markdown file
    qc_markdown = "anteater.Rmd"
  ),

  zebra = list(
    signals = dplyr::tibble( # county case forecaster
      data_source = c("jhu-csse",
                      ## "usa-facts",
                      "fb-survey",
                      "doctor-visits"
                      ),
      signal = c("confirmed_incidence_num", "smoothed_hh_cmnty_cli", "smoothed_cli"),
      start_day = animalia::grab_start_day(aheads, 28, 28, "epiweek")(forecast_date),
      geo_type = "county"
    ),
    ## The corresponding QC markdown file
    qc_markdown = "zebra.Rmd"
  )
)

## These should ideally be grouped

# State specific sets -----------------------------------------------------

## Here is where you choose the forecasters you use for state and counties
state_forecaster_name  <- "anteater"
county_forecaster_name  <- "zebra"

## Start of DO NOT MODIFY: the next two lines are a consequence of the previous two lines
state_forecaster_signals  <- forecaster_details[[state_forecaster_name]][["signals"]]
county_forecaster_signals  <- forecaster_details[[county_forecaster_name]][["signals"]]

if (is.null(state_forecaster_signals) || is.null(county_forecaster_signals)) {
  stop("Forecaster details misconfigured! Please fix and rerun")
}
## End of DO NOT MODIFY


state_corrections_params <- zookeeper::default_state_params(
  # many other options, see the function documentation
  data_source = state_forecaster_signals$data_source,
  signal = state_forecaster_signals$signal,
  geo_type = state_forecaster_signals$geo_type)

state_corrector <- zookeeper::make_state_corrector(
  params = state_corrections_params,
  # data, locations, times to do special correction processing
  manual_flags = tibble::tibble(
    data_source = "jhu-csse",
    signal = c(rep("deaths_incidence_num", 3), "confirmed_incidence_num"),
    geo_value = c("va","ky","ok","ok"),
    time_value = list(
      seq(lubridate::ymd("2021-02-21"), lubridate::ymd("2021-03-04"), by = 1),
      lubridate::ymd(c("2021-03-18","2021-03-19")),
      lubridate::ymd("2021-04-07"),
      lubridate::ymd("2021-04-07")),
    max_lag = rep(90, 4)) # how far do we back distribute these spikes?
  )

state_forecaster_args <- list(
  ahead = aheads,
  lags = c(0,7,14),
  tau = evalcast::covidhub_probs(), # 23 quantiles
  lambda = 0, # no regularization or CV
  lp_solver = "gurobi", # can remove if no license
  noncross = TRUE, # takes a bit longer, but not much
  featurize = animalia::make_state_7dav_featurizer(), # has no arguments
  verbose = TRUE,
  save_wide_data = file.path(output_dir, state_output_subdir),
  save_trained_models = file.path(output_dir, state_output_subdir)
)


# County specific sets ----------------------------------------------------

county_corrections_params  <- zookeeper::default_county_params(
  data_source = county_forecaster_signals$data_source,
  signal = county_forecaster_signals$signal[1] # only correct cases
)

county_corrector  <- zookeeper::make_county_corrector(params = county_corrections_params)

prob_type <- ifelse(county_forecaster_signals$signal[1] == "confirmed_incidence_num",
                    "inc_case", "standard")

county_forecaster_args <- list(
  ahead = aheads,
  lags = list(c(0, 1, 2, seq(3, 21, 3)), seq(3,28,7), seq(3,28,7)),
  tau = evalcast::covidhub_probs(type = prob_type), # only 7 quantiles for inc_cases
  lambda = 0,
  lp_solver = "gurobi",
  noncross = TRUE,
  featurize = animalia::make_county_7dav_featurizer(
    response_data_source = county_forecaster_signals$data_source[1],
    response_signal = county_forecaster_signals$signal[1],
    n_locations = n_counties
  ),
  verbose = TRUE,
  save_wide_data = file.path(output_dir, county_output_subdir),
  save_trained_models = file.path(output_dir, county_output_subdir)
)

