devtools::install()
library(tidyverse)

zz <- zyzzyva::get_forecasters(n_locations = 20,
                               weeks_back = 4)$zyzzyva_covidcast$forecaster
signals <- tibble(
    data_source = c("usa-facts",
                    "fb-survey",
                    "indicator-combination"),
    signal = c("confirmed_incidence_num",
               "smoothed_hh_cmnty_cli",
               "nmf_day_doc_fbc_fbs_ght")
)
new_results <- evalcast::get_predictions(forecaster=zz,
                                         name_of_forecaster="zyzzyva",
                                         signals=signals,
                                         forecast_dates=as.Date("2021-01-25"),
                                         incidence_period="epiweek",
                                         ahead=1,
                                         geo_type="state",
                                         geo_values="*") %>%
                    as_tibble()
