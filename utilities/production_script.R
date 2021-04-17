#!/usr/bin/env Rscript

## ## Obtain environment variables

## forecast_date <- lubridate::ymd(Sys.getenv("FORECAST_DATE"))
## today  <- lubridate::ymd(Sys.getenv("TODAY"))
## ##output_dir  <- Sys.getenv("OUTPUT_DIR")
## ## We can fix this at /mnt
## output_dir  <- "/mnt"

## These are usually not changed except by those know what is going on.
## Volatile packages are those that go frequent development and are
## therefore installed just-in-time from repos before running forecasts

volatile_pkgs <- list(
  covidcast = list(repo = "cmu-delphi/covidcast", ref = "evalcast",
                   subdir = "R-packages/evalcast"),
  evalcast = list(repo = "cmu-delphi/covidcast", ref = "modeltools",
                  subdir = "R-packages/modeltools"),
  zookeeper = list(repo = "cmu-delphi/covid-19-forecast", ref = "develop",
                   subdir = "utilities/zookeeper"),
  animalia = list(repo = "cmu-delphi/covid-19-forecast", ref = "develop",
                  subdir = "forecasters/animalia")
)

for (pkg in volatile_pkgs) {
  devtools::install_github(repo = pkg$repo, ref = pkg$ref,
                           subdir = pkg$subdir, upgrade = "never")
}

source("production_params.R")

## Ensure that state and county output directories specified in production_params.R
## exist in the working directory
if (!fs::dir_exists(county_output_dir)) {
  message(sprintf("Creating non-existent county output directory %s", county_output_dir))
  fs::dir_create(path = county_output_dir)
}

if (!fs::dir_exists(state_output_dir)) {
  message(sprintf("Creating non-existent state output directory %s", state_output_dir))
  fs::dir_create(path = state_output_dir)
}

cat(sprintf("Forecast date: %s\n", forecast_date))

library(tidyverse)

cat("Running States\n")

## make_aardvark_corrector below uses the signals defaults. Arrange to use the parameter

state_predictions <- evalcast::get_predictions(
  forecaster = animalia::production_forecaster,
  name_of_forecaster = state_forecaster_name,
  signals = state_forecaster_signals,
  forecast_dates = forecast_date,
  incidence_period = "epiweek",
  apply_corrections = state_corrector,
  forecaster_args = state_forecaster_args
)
warnings()


cat("Writing State results\n")
## Write result
saveRDS(state_predictions,
        file = file.path(
          output_dir,
          state_output_subdir,
          sprintf("predictions_for_%s.RDS", forecast_date)
        ))

cat("Running Counties\n")


county_predictions <- evalcast::get_predictions(
  forecaster = animalia::production_forecaster,
  name_of_forecaster = county_forecaster_name,
  signals = county_forecaster_signals,
  forecast_dates = forecast_date,
  incidence_period = "epiweek",
  apply_corrections = county_corrector,
  forecaster_args = county_forecaster_args
)
warnings()


cat("Writing county results\n")
## Write result
saveRDS(county_predictions,
        file = file.path(output_dir, county_output_subdir,
                         sprintf("predictions_for_%s.RDS", forecast_date)))

cat("Running QA for States\n")

## Render the QA report
state_qc_md  <- forecaster_signals[[state_forecaster_name]][["qc_markdown"]]
rmarkdown::render(input = state_qc_md,
                  output_file = sprintf("%s_%s.html", state_forecaster_name, forecast_date),
                  output_dir = file.path(output_dir, state_output_subdir))

cat("Done with States\n")
cat("Running QA for Counties\n")

## Render the QA report
county_qc_md  <- forecaster_signals[[county_forecaster_name]][["qc_markdown"]]
rmarkdown::render(input = county_qc_md,
                  output_file = sprintf("%s_%s.html", county_forecaster_name, forecast_date),
                  output_dir = file.path(output_dir, county_output_subdir))

cat("Done with Counties\n")

cat("Combine submission files")
combined <- bind_rows(state_predictions, county_predictions)
combined <- zookeeper::format_predictions_for_reichlab_submission(combined)

readr::write_csv(
  combined,
  file = file.path(output_dir, sprintf("%s-CMU-TimeSeries.csv", forecast_date))
)

## Save session info
sessionInfo()
