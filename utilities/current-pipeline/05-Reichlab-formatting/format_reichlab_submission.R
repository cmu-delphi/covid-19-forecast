#-----------------------------------------------------#
# 
#-----------------------------------------------------#
path_to_production_run_output <- "/Users/dajmcdon/Documents/COVID-Delphi/covidcast-forecast/forecaster_predictions"
data_date <- lubridate::ymd("2020-11-02")
aheads <- 4:1
response_state <- "usa-facts_deaths_incidence_num"
response_cty <- "usa-facts_confirmed_incidence_num"
incidence_period <- "epiweek"
n_locations <- 200

library(purrr)
library(readr)
library(tidyverse) # JS: added - probably could reduce
library(logger)
source("format.R")


geo_type <- "state"
forecaster <- "aardvark_cookies_and_cream"


# (1) Load data
predictions_cards <- list()
for(ahead in aheads){
  path_to_prediction_card <- file.path(path_to_production_run_output,
                                       data_date,ahead,response_state,geo_type,
                                       incidence_period,n_locations,forecaster,
                                       "out.RDS")
  prediction_card <- readRDS(path_to_prediction_card)
  predictions_cards[[ahead]] <- prediction_card
}

# (2) Reformat data
state_preds <- format_predictions_cards_for_reichlab_submission(predictions_cards)

# (3) Write to csv
forecast_date <- state_preds[["forecast_date"]][1] # Output of format_predictions_cards_for_reichlab_submission guarantees they will all be the same.




geo_type <- "county"
forecaster <- "zyzzyva_covidcast"
#-----------------------------------------------------#

# (1) Load data
predictions_cards <- list()
for(ahead in aheads){
  path_to_prediction_card <- file.path(path_to_production_run_output,
                                       data_date,ahead,response_cty,geo_type,
                                       incidence_period,n_locations,forecaster,
                                       "out.RDS")
  prediction_card <- readRDS(path_to_prediction_card)
  predictions_cards[[ahead]] <- prediction_card
}

# (2) Reformat data
county_preds <- format_predictions_cards_for_reichlab_submission(predictions_cards, is_case=TRUE)

out_file_name <- paste0(forecast_date, "-CMU-TimeSeries",".csv")

log_info(sprintf("Writing to file %s",out_file_name))

all_preds = bind_rows(state_preds, county_preds)

write_csv(all_preds, file=out_file_name)

