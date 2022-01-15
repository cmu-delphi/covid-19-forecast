output_dir <- here::here()
state_output_subdir <- here::here("hosp-states")

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

`%magrittr>%` <- magrittr::`%>%`

## End of prologue
forecast_date <- lubridate::ymd("2022-01-10")
today  <- lubridate::ymd(Sys.Date())


aheads  <- 0:28
qa_lookback <- 60 # how far back do we show actual data on the QA report?
correction_lookback <- 90 # how far back do we look on the daily corrections report?
forecaster_details  <- list(
  
  antelope = list(
    signals = dplyr::tibble( # state death forecaster
      data_source = c("hhs", "jhu-csse"),
      signal = c("confirmed_admissions_covid_1d",
                 "confirmed_incidence_num"
      ),
      start_day = lubridate::ymd("2021-01-01"),
      geo_type = "state"
    ),
    ## The corresponding QC markdown file
    qc_markdown = "antelope.Rmd"
  ))

state_corrections_md <- "state-corrections.Rmd"
state_forecaster_name  <- "antelope"

state_forecaster_signals  <- forecaster_details[[state_forecaster_name]][["signals"]]
if (is.null(state_forecaster_signals)) {
  stop("Forecaster details misconfigured! Please fix and rerun")
}

state_corrections_params <- zookeeper::default_state_params(
  # many other options, see the function documentation
  data_source = state_forecaster_signals$data_source,
  signal = state_forecaster_signals$signal,
  geo_type = state_forecaster_signals$geo_type)

state_corrector <- zookeeper::make_state_corrector(params = state_corrections_params)
state_forecaster_args <- list(
  ahead = aheads,
  lags = c(0,7,14),
  tau = evalcast::covidhub_probs(), # 23 quantiles
  lambda = 0, # no regularization or CV
  lp_solver = "gurobi", # can remove if no license
  noncross = TRUE, # takes a bit longer, but not much
  featurize = animalia::make_7dav_featurizer(), # has no arguments
  verbose = TRUE,
  signals_to_normalize = c(TRUE, TRUE),
  save_wide_data = here::here(state_output_subdir),
  save_trained_models = here::here(state_output_subdir)
)
