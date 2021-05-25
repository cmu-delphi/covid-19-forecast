#!/usr/bin/env Rscript

## Obtain production parameters
source("production_params.R")

## Ensure that state and county output directories specified in production_params.R
## exist in the working directory
if (!fs::dir_exists(county_output_subdir)) {
  message(sprintf("Creating non-existent county output directory %s", county_output_subdir))
  fs::dir_create(path = county_output_subdir)
}

if (!fs::dir_exists(state_output_subdir)) {
  message(sprintf("Creating non-existent state output directory %s", state_output_subdir))
  fs::dir_create(path = state_output_subdir)
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


cat("Running Corrections report for states\n")

## Render the corrections report

rmarkdown::render(input = state_corrections_md,
                  output_file = sprintf("%s_%s.html", tools::file_path_sans_ext(state_corrections_md), forecast_date),
                  output_dir = file.path(output_dir, state_output_subdir))

cat("Running QA for States\n")

## Render the QA report
state_qc_md  <- forecaster_details[[state_forecaster_name]][["qc_markdown"]]
rmarkdown::render(input = state_qc_md,
                  output_file = sprintf("%s_%s.html", state_forecaster_name, forecast_date),
                  output_dir = file.path(output_dir, state_output_subdir))

cat("Done with States\n")

cat("Running Corrections report for counties\n")

## Render the corrections report
rmarkdown::render(input = county_corrections_md,
                  output_file = sprintf("%s_%s.html", tools::file_path_sans_ext(county_corrections_md), forecast_date),
                  output_dir = file.path(output_dir, county_output_subdir))

cat("Running QA for Counties\n")

## Render the QA report
county_qc_md  <- forecaster_details[[county_forecaster_name]][["qc_markdown"]]
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
