## Start of prologue
## These are usually not changed except by those know what is going
## on.  Volatile packages are those that undergo frequent development
## and are therefore installed just-in-time from repos before running
## forecasts

## Install a package if not already installed from github
install_from_github_if_needed <- function(pkgs, ...) {
  installed_pkgs <- installed.packages()[, 1]
  pkg_names <- names(pkgs)
  to_install <- pkgs[setdiff(pkg_names, installed_pkgs)]
  for (pkg in to_install) {
    devtools::install_github(repo = pkg$repo, ref = pkg$ref,
                             subdir = pkg$subdir, ...)
  }
}

volatile_pkgs <- list(
  evalcast = list(repo = "cmu-delphi/covidcast", ref = "evalcast",
                   subdir = "R-packages/evalcast"),
  modeltools = list(repo = "cmu-delphi/covidcast", ref = "modeltools",
                  subdir = "R-packages/modeltools"),
  zookeeper = list(repo = "cmu-delphi/covid-19-forecast", ref = "develop",
                   subdir = "utilities/zookeeper"),
  animalia = list(repo = "cmu-delphi/covid-19-forecast", ref = "develop",
                  subdir = "forecasters/animalia")
)

install_from_github_if_needed(volatile_pkgs, upgrade = "never")

## End of prologue

## Parameters section proper

## Obtain environment variables One place for all parameters used by
## all production scripts (production_script.R) and also markdowns
## (anteater.Rmd zebra.Rmd state-corrections.Rmd
## county-corrections.Rmd)

forecast_date <- lubridate::ymd(Sys.getenv("FORECAST_DATE"))
today  <- lubridate::ymd(Sys.getenv("TODAY"))

##output_dir  <- Sys.getenv("OUTPUT_DIR")
## We can fix this at /mnt
output_dir  <- "/mnt"

aheads  <- 1:4
qa_lookback <- 60 # how far back do we show actual data on the QA report?
correction_lookback <- 90 # how far back do we look on the daily corrections report?
n_counties <- 200 # we predict the top 200 counties


## Subdirectories for states and counties

state_output_subdir  <- "state-output"
county_output_subdir <- "county-output"

## Here is where you change the signals if one or the other is not available,
## or even add other replacement forecasters
forecaster_details  <- list(

  anteater = list(
    signals = dplyr::tibble( # state death forecaster
      data_source = "jhu-csse",
      signal = c("deaths_incidence_num",
                 "confirmed_incidence_num"
                 ),
      start_day = lubridate::ymd("2020-06-01"),
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
      start_day = lubridate::ymd("2020-06-01"),
      geo_type = "county"
    ),
    ## The corresponding QC markdown file
    qc_markdown = "zebra.Rmd"
  )
)

state_corrections_md <- "state-corrections.Rmd"
county_corrections_md <- "county-corrections.Rmd"


## These should ideally be grouped

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

# State specific sets -----------------------------------------------------

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
    signal = c(rep("deaths_incidence_num", 3),
               "confirmed_incidence_num",
               ## from JHU-CSSE notes 2021-04-17, 2021-04-18
               "deaths_incidence_num",
               "deaths_incidence_num",
               "confirmed_incidence_num",
               "confirmed_incidence_num"#,
               ## ## from JHU-CSSE notes 2021-04-24, 2021-04-25
               ## "confirmed_incidence_num"
               ),
    geo_value = c("va","ky","ok","ok",
                  ## from JHU-CSSE notes 2021-04-17, 2021-04-18
                  "ak","mi","mo","al"#,
                  ## ## from JHU-CSSE notes 2021-04-24, 2021-04-25
                  ## "al"
                  ),
    time_value = list(
      seq(lubridate::ymd("2021-02-21"), lubridate::ymd("2021-03-04"), by = 1),
      lubridate::ymd(c("2021-03-18","2021-03-19")),
      lubridate::ymd("2021-04-07"),
      lubridate::ymd("2021-04-07"),
      ## from JHU-CSSE notes 2021-04-17, 2021-04-18
      lubridate::ymd("2021-04-15"),
      lubridate::ymd(c("2021-04-01", "2021-04-03", "2021-04-06", "2021-04-08", "2021-04-10", "2021-04-13", "2021-04-15", "2021-04-17",
                       ## ongoing as of 2021-04-24
                       "2021-04-20", "2021-04-22", "2021-04-24"
                       )),
      lubridate::ymd("2021-04-17"),
      lubridate::ymd("2021-04-13","2021-04-20")#,
      ## ## from JHU-CSSE notes 2021-04-24, 2021-04-25
      ## lubridate::ymd("2021-04-20")
    ),
    max_lag = c(rep(90, 4),
                ## from JHU-CSSE notes 2021-04-17, 2021-04-18
                75, 150, 150, 180#,
                ## from JHU-CSSE notes 2021-04-24, 2021-04-25
                ## as.integer(as.Date("2021-04-20") - as.Date("2020-10-23"))
                )
  )
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
  signals_to_normalize = c(TRUE, TRUE),
  save_wide_data = file.path(output_dir, state_output_subdir),
  save_trained_models = file.path(output_dir, state_output_subdir)
)


# County specific sets ----------------------------------------------------

county_corrections_params  <- zookeeper::default_county_params(
  data_source = county_forecaster_signals$data_source,
  signal = county_forecaster_signals$signal[1] # only correct cases
)

county_corrector  <- zookeeper::make_county_corrector(
  params = county_corrections_params,
  manual_flags = tibble::tibble(
    data_source = "jhu-csse",
    signal = "confirmed_incidence_num",
    geo_value = c(
      ## from JHU-CSSE notes 2021-04-17, 2021-04-18
      "29077", "29095", "29183", "29189",
      "01097"#,
      ## ## from JHU-CSSE notes 2021-04-24, 2021-04-25 --- seems to conflict with last week's
      ## "01097"
    ),
    time_value = list(
      ## from JHU-CSSE notes 2021-04-17, 2021-04-18
      lubridate::ymd(c("2021-03-11","2021-04-17")), lubridate::ymd(c("2021-03-11","2021-04-17")), lubridate::ymd(c("2021-03-11","2021-04-17")),
      lubridate::ymd("2021-04-17"),
      lubridate::ymd("2021-04-13",
                     ## partially resolve conflict
                     "2021-04-20"
                     )#,
      ## ## from JHU-CSSE notes 2021-04-24, 2021-04-25 --- seems to conflict with last week's
      ## lubridate::ymd("2021-04-20")
    ),
    max_lag = c(
      ## from JHU-CSSE notes 2021-04-17, 2021-04-18
      rep_len(150, 4L),
      180#,
      ## ## from JHU-CSSE notes 2021-04-24, 2021-04-25 --- seems to conflict with last week's
      ## as.integer(as.Date("2021-04-20") - as.Date("2020-10-23"))
    )
  )
)

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
  #signals_to_normalize = c(TRUE, FALSE, FALSE), 
  verbose = TRUE,
  save_wide_data = file.path(output_dir, county_output_subdir),
  save_trained_models = file.path(output_dir, county_output_subdir)
)

