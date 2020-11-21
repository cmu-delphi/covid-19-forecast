
<!-- README.md is generated from README.Rmd. Please edit that file -->

# armadillo

<!-- badges: start -->

<!-- badges: end -->

The goal of armadillo is to implement Larry and Valerie’s mobility model
as a forecaster working with evalcast package.

## Installation

## Example

This is a basic example:

``` r
library(armadillo)

signals <- tibble::tibble(data_source = c("jhu-csse","safegraph"),
signal = c("deaths_incidence_num","completely_home_prop"),
start_day = c("2020-03-08", "2020-03-01"))
arma_forecaster <- Armadillo_forecaster(
  before_pan = TRUE,
  mob_shift = 1,
  mob_fun = "min",
  DC = NULL,
  initial_fun = function(x) {
    max(x) * 0.6
  },
  initial_val = c(0, 0, 0, 1),
  lower = c(0, -40, -4, -2, 0.00001),
  upper = c(Inf, 40, 4, 2, 2),
  t0 = 30000, r = 0.95, nlimit = 2000
)
res_armadillo <- evalcast::get_predictions(arma_forecaster,
  name_of_forecaster = "armadillo",
  signals,
  forecast_dates = "2020-07-20",
  incidence_period = "epiweek",
  ahead = 1:3,
  geo_type = "state",
  geo_values = "*"
)
eva <- evalcast::evaluate_predictions(res_armadillo)
```
