# Templates for retrospective evaluation of production forecaster

## `state_production_notebook.Rmd` / `county_production_notebook.Rmd`

These notebooks compare the production forecasters (state deaths and county cases resp.) against submissions to COVIDhub. They can be run without any modifications, except possibly the `forecast_dates` variable which defines which dates forecasts should be provided for.

## `state_exploration_notebook.Rmd` / `county_exploration_notebook.Rmd`

These notebooks are for comparing small tweaks of the production forecaster against the production forecaster itself and submissions to COVIDhub.

The notebooks are an example where we compare 3 small tweaks; if you have a different number of tweaked forecasters you want to compare, you can amend the notebook accordingly.

For each tweaked forecaster, we load the production forecaster parameters from the production script (`product_params.R`). These are essentially the arguments for the `evalcast::get_predictions()` call. Tweaks are made by amending these arguments.
