# Script to run multiple versions of a model.
#
# To run, use the following command from the prompt:
#
#     Rscript compare_zyzzyva_models.R [--analysis_only]
#
# where the `--analysis_only` flag indicates that the predictions and evaluations can be read from
# `compare_models.Rda` and only the analysis is rerun.

library(tidyverse)
library(evalcast)
library(zyzzyva)
library(lubridate)
library(optparse)

#######################
# Variables to change #
#######################
GEO_TYPE <- "county"
DATES <- as.Date("2021-01-01")
AHEAD <- 1:4
SIGNALS <- list(
    production = tibble(
        data_source = c("usa-facts",
                        "fb-survey",
                        "indicator-combination"),
        signal = c("confirmed_incidence_num",
                   "smoothed_hh_cmnty_cli",
                   "nmf_day_doc_fbc_fbs_ght")
    ),
    alternate = tibble(
        data_source = c("usa-facts",
                        "fb-survey",
                        "doctor-visits"),
        signal = c("confirmed_incidence_num",
                   "smoothed_hh_cmnty_cli",
                   "smoothed_cli")
    ),
    alternate_adj = tibble(
        data_source = c("usa-facts",
                        "fb-survey",
                        "doctor-visits"),
        signal = c("confirmed_incidence_num",
                   "smoothed_hh_cmnty_cli",
                   "smoothed_adj_cli")
    )
)

##########################
# Command line arguments #
##########################
opt_list <- list(
    make_option(c("--analysis_only"), action="store_true", default=FALSE,
                help="whether to skip the predictions and evaluations")
)
opt <- parse_args(OptionParser(option_list=opt_list))

######################
# Set up forecasters #
######################
if (opt$analysis_only) {
    load("compare_models.Rda")
} else {
    zz <- zyzzyva::get_forecasters(n_locations = 200,
                                   weeks_back = 4)$zyzzyva_covidcast$forecaster
    forecast <- function(forecaster_name, ahead) {
        print(now()) # to track progress
        evalcast::get_predictions(forecaster = zz,
                                  name_of_forecaster = forecaster_name,
                                  signals = SIGNALS[[forecaster_name]],
                                  forecast_dates = DATES,
                                  incidence_period = "epiweek",
                                  ahead = ahead,
                                  geo_type = GEO_TYPE,
                                  geo_values = "*")
    }

    predictions <- tibble()
    class(predictions) <- c("predictions_cards", class(predictions))
    for (forecaster_name in names(SIGNALS)) {
        for (ahead in AHEAD) {
            predictions <- rbind(predictions, forecast(forecaster_name, ahead))
        }
    }
    print(now()) # to track progress
    evals <- evalcast::evaluate_predictions(predictions, geo_type = GEO_TYPE)
    save(evals, predictions, file="compare_models.Rda")
}

###############
# Do analysis #
###############

# Sum of WIS across all forecast locations
total_wis_diffs <- evals %>%
    group_by(forecast_date, forecaster) %>%
    summarise(total_wis = sum(wis, rm.na = T)) %>%
    pivot_wider(names_from = "forecast_date", values_from = "total_wis")

# Comparisons against baseline
wis_diffs <- evals %>%
    filter(forecaster == "production") %>%
    select(forecast_date, geo_value, ahead, wis) %>%
    inner_join(filter(evals, forecaster != "production"),
               by = c("forecast_date", "geo_value", "ahead"),
               suffix = c("_old", "_new")) %>%
    mutate(wis_ratio = wis_new / wis_old,
           is_improvement = (wis_new <= wis_old)) %>% 
    group_by(forecast_date, forecaster, ahead) %>%
    summarise(num_improvements = sum(is_improvement),
              geo_mean_wis_ratio = exp(mean(log(wis_ratio))))

print(total_wis_diffs)
print(wis_diffs, width = Inf)
