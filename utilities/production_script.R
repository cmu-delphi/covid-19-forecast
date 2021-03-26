#!/usr/bin/env Rscript

## Obtain environment variables

forecast_date <- lubridate::ymd(Sys.getenv("FORECAST_DATE"))
today  <- lubridate::ymd(Sys.getenv("TODAY"))
##output_dir  <- Sys.getenv("OUTPUT_DIR")
## We can fix this at /mnt
output_dir  <- "/mnt"

## Install evalcast from the evalcast branch
devtools::install_github("cmu-delphi/covidcast", ref = "evalcast",
                         subdir = "R-packages/evalcast", upgrade = "never")

## Install evalcast from the evalcast branch
devtools::install_github("cmu-delphi/covidcast", ref = "modeltools",
                         subdir = "R-packages/modeltools", upgrade = "never")

## Install the latest zookeeper
devtools::install_github("cmu-delphi/covid-19-forecast", ref = "develop",
                         subdir = "utilities/zookeeper", upgrade = "never")

## Install the latest animalia
devtools::install_github("cmu-delphi/covid-19-forecast", ref = "develop",
                         subdir = "forecasters/animalia", upgrade = "never")


cat(sprintf("Forecast date: %s, Output dir: %s\n", forecast_date, output_dir))

library(tidyverse)

cat("Running States\n")

aheads  <- 1:4
start_day <- animalia::grab_start_day(aheads, 28, 14, "epiweek")(forecast_date)

signals <- dplyr::tibble(
  data_source = "jhu-csse",
  signal = c("deaths_incidence_num", 
             "confirmed_incidence_num"),
  start_day = start_day,
  geo_type = "state")



aa_corrector  <- zookeeper::make_aardvark_corrector()
state_forecaster_args <- list(
  ahead = aheads,
  lags = c(0,7,14),
  tau = evalcast::covidhub_probs(),
  lambda = 0,
  featurize = animalia::states_featurizer,
  verbose = TRUE,
  save_wide_data = file.path(output_dir, "state-output"),
  save_trained_models = file.path(output_dir, "state-output")
)
state_predictions <- evalcast::get_predictions(
  forecaster = animalia::production_forecaster,
  name_of_forecaster = "anteater",
  signals = signals,
  forecast_dates = forecast_date,
  incidence_period = "epiweek",
  apply_corrections = aa_corrector,
  forecaster_args = state_forecaster_args
)
warnings()


cat("Writing State results\n")
## Write result
saveRDS(state_predictions, 
        file = file.path(output_dir, "state-output", 
                         sprintf("predictions_for_%s.RDS", forecast_date)))

cat("Running QA for States\n")

## Render the QA report
rmarkdown::render("anteater.Rmd")

cat("Done with States\n")
cat("Running Counties\n")


start_day <- animalia::grab_start_day(aheads, 28, 28, "epiweek")(forecast_date)
zz_corrector  <- zookeeper::make_zyzzyva_corrector(
  zookeeper::default_county_params(data_source = "jhu-csse"))
signals <- tibble(
  data_source = c(
    ## "usa-facts",
    "jhu-csse",
    "fb-survey",
    "doctor-visits"),
  signal = c(
    "confirmed_incidence_num",
    ## "deaths_incidence_num",
    "smoothed_hh_cmnty_cli",
    "smoothed_cli"),
  start_day = start_day,
  geo_type = "county"
)

county_forecaster_args <- list(
  ahead = aheads,
  lags = list(c(0, 1, 2, seq(3, 21, 3)), seq(3,28,7), seq(3,28,7)), # Lags
  tau = evalcast::covidhub_probs(type = "inc_case"),
  lambda = 0,
  featurize = animalia::counties_featurizer,
  verbose = TRUE,
  save_wide_data = file.path(output_dir, "county-output"),
  save_trained_models = file.path(output_dir, "county-output")
)
county_predictions <- evalcast::get_predictions(
  forecaster = production_forecaster,
  name_of_forecaster = "zebra",
  signals = signals,
  forecast_dates = forecast_date,
  incidence_period = "epiweek",
  apply_corrections = zz_corrector,
  forecaster_args = county_forecaster_args
)
warnings()



cat("Writing county results\n")
## Write result
saveRDS(county_predictions, 
        file = file.path(output_dir, "county-output",
                         sprintf("predictions_for_%s.RDS", forecast_date)))

cat("Running QA for Counties\n")

## Render the QA report
rmarkdown::render("zebra.Rmd")

cat("Done with Counties\n")

cat("Combine submission files")

combined <- bind_rows(state_predictions, county_predictions)
combined <- zookeeper::format_predictions_for_reichlab_submission(combined)

readr::write_csv(combined, 
                 file = file.path(
                   output_dir, 
                   sprintf("%s-CMU-TimeSeries.csv", forecast_date)))

## Save session info
sessionInfo()