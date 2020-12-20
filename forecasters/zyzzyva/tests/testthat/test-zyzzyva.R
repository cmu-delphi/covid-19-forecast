# devtools::install("~/Fellowship/repos/delphi/covidcast/R-packages/evalcast/")
devtools::install("~/Fellowship/repos/delphi/covid-19-forecast/forecasters/zyzzyva")
library(tidyverse)

zz <- zyzzyva::get_forecasters("jhu-csse_deaths_incidence_num", "state")$zyzzyva_covidcast$forecaster
signals <- tibble(
    data_source = c("jhu-csse",
                    "usa-facts",
                    "fb-survey",
                    "indicator-combination"),
    signal = c("deaths_incidence_num",
               "confirmed_incidence_num",
               "smoothed_hh_cmnty_cli",
               "nmf_day_doc_fbc_fbs_ght")
)
evalcast::get_predictions(forecaster=zz,
                          name_of_forecaster="zyzzyva",
                          signals=signals,
                          forecast_dates=as.Date("2020-10-05"),
                          incidence_period="epiweek",
                          ahead=1,
                          geo_type="state",
                          geo_values="*")
