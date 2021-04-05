#!/usr/bin/env Rscript

## ## Obtain environment variables

## forecast_date <- lubridate::ymd(Sys.getenv("FORECAST_DATE"))
## today  <- lubridate::ymd(Sys.getenv("TODAY"))
## ##output_dir  <- Sys.getenv("OUTPUT_DIR")
## ## We can fix this at /mnt
## output_dir  <- "/mnt"


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

source("params.R")

cat(sprintf("Forecast date: %s, Output dir: %s\n", forecast_date, output_dir))

library(tidyverse)

cat("Running States\n")

aheads  <- 1:4

## make_aardvark_corrector below uses the signals defaults. Arrange to use the parameter
anteater_signals  <- forecaster_signals[["anteater"]]
state_params <- zookeeper::default_state_params(data_source = anteater_signals$data_source,
                                                signal = anteater_signals$signal,
                                                geo_type = anteater_signals$geo_type)
aa_corrector  <- zookeeper::make_aardvark_corrector(params = state_params)
state_forecaster_args <- list(
  ahead = aheads,
  lags = c(0,7,14),
  tau = evalcast::covidhub_probs(),
  lambda = 0,
  featurize = animalia::make_state_7dav_featurizer(),
  verbose = TRUE,
  save_wide_data = file.path(output_dir, state_output_subdir),
  save_trained_models = file.path(output_dir, state_output_subdir)
)
state_predictions <- evalcast::get_predictions(
  forecaster = animalia::production_forecaster,
  name_of_forecaster = "anteater",
  signals = anteater_signals,
  forecast_dates = forecast_date,
  incidence_period = "epiweek",
  apply_corrections = aa_corrector,
  forecaster_args = state_forecaster_args
)
warnings()


cat("Writing State results\n")
## Write result
saveRDS(state_predictions,
        file = file.path(output_dir, state_output_subdir,
                         sprintf("predictions_for_%s.RDS", forecast_date)))

cat("Running Counties\n")


zebra_signals  <- forecaster_signals[["zebra"]]
county_params  <- zookeeper::default_county_params(data_source = zebra_signals$data_source)
zz_corrector  <- zookeeper::make_zyzzyva_corrector(params = county_params)

county_forecaster_args <- list(
  ahead = aheads,
  lags = list(c(0, 1, 2, seq(3, 21, 3)), seq(3,28,7), seq(3,28,7)), # Lags
  tau = evalcast::covidhub_probs(type = "inc_case"),
  lambda = 0,
  featurize = animalia::make_county_7dav_featurizer(),
  verbose = TRUE,
  save_wide_data = file.path(output_dir, county_output_subdir),
  save_trained_models = file.path(output_dir, county_output_subdir)
)
county_predictions <- evalcast::get_predictions(
  forecaster = animalia::production_forecaster,
  name_of_forecaster = "zebra",
  signals = zebra_signals,
  forecast_dates = forecast_date,
  incidence_period = "epiweek",
  apply_corrections = zz_corrector,
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
rmarkdown::render(input = "anteater.Rmd", output_file = sprintf("anteater_%s.html", forecast_date),
                  output_dir = file.path(output_dir, state_output_subdir))

cat("Done with States\n")
cat("Running QA for Counties\n")

## Render the QA report
rmarkdown::render(input = "zebra.Rmd", output_file = sprintf("zebra_%s.html", forecast_date),
                  output_dir = file.path(output_dir, county_output_subdir))

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
