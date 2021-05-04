library(covidcast)
library(evalcast)
library(modeltools)
library(dplyr)

## Setup

# What are we forecasting?
data_source <- "jhu-csse"
data_signals <- c("deaths_7dav_incidence_num", "confirmed_7dav_incidence_num")
our_pred_dates <- NULL #get_covidhub_forecast_dates("CMU-TimeSeries")
forecast_dates <- lubridate::ymd("2020-09-28") #our_pred_dates[seq(7, 28, by=4)]
# 6 forecast dates, always on 2nd day of the epiweek
incidence_period <- "day"
ahead <- c(7, 14, 21, 28) - 2 # 5 aheadmbecause we predict the current week
geo_type <- "state"


# Some quantgen parameters
n <- 28               # Training set size (in days)
lags <- c(0, 7, 14)   # Lags (in days) for features
# lp_solver <- "gurobi"
sort <- TRUE
nonneg <- TRUE


# For the autoregressive models fit by quantgen, we need to ensure that we pull
# enough training data so that 1. we actually have the response (defined by some
# number of days ahead into the future) and 2. we have the lagged features
start_day_quantgen <- function(forecast_date) {
  return(as.Date(forecast_date) - max(ahead) - n - max(lags) + 1)
}

qar_dc <- get_predictions(
  forecaster = quantgen_forecaster,
  name_of_forecaster = "QAR3_D+C",
  signals = tibble::tibble(
    data_source = data_source,
    signal = data_signals,
    start_day = list(start_day_quantgen)),
  forecast_dates = forecast_dates,
  as_of_override = function(x) lubridate::ymd("2021-02-19"),
  incidence_period = incidence_period,
  ahead = ahead, geo_type = "state",
  signal_aggregation = "list",
  geo_values = "*",
  n = n,
  lags = lags, # optionally use a list for different lags by
  lambda = 0, # Just do quantile regression
  sort = sort, nonneg = nonneg)
