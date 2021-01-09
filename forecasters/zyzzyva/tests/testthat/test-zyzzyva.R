devtools::install()
library(tidyverse)

zz <- zyzzyva::get_forecasters("usa-facts_confirmed_incidence_num", "county")$zyzzyva_covidcast$forecaster
signals <- tibble(
    data_source = c("usa-facts",
                    "jhu-csse",
                    "fb-survey",
                    "indicator-combination"),
    signal = c("confirmed_incidence_num",
               "deaths_incidence_num",
               "smoothed_hh_cmnty_cli",
               "nmf_day_doc_fbc_fbs_ght")
)
x <- evalcast::get_predictions(forecaster=zz,
                               name_of_forecaster="zyzzyva",
                               signals=signals,
                               forecast_dates=as.Date("2020-10-05"),
                               incidence_period="epiweek",
                               ahead=1,
                               geo_type="county",
                               geo_values="*")
