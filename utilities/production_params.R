## Obtain environment variables

## One place for all parameters used by all production scripts (production_script.R) and
## and also markdowns (anteater.Rmd zebra.Rmd)

forecast_date <- lubridate::ymd(Sys.getenv("FORECAST_DATE"))
today  <- lubridate::ymd(Sys.getenv("TODAY"))
##output_dir  <- Sys.getenv("OUTPUT_DIR")
## We can fix this at /mnt
output_dir  <- "/mnt"

state_output_subdir  <- "state-output"
county_output_subdir <- "county-output"

aheads  <- 1:4

## Signals used by various forecasters
## Here is where you change the signals if one or the other is not available.
forecaster_signals <- list(
  anteater = dplyr::tibble(data_source = "jhu-csse",
                           signal = c("deaths_incidence_num",
                                      "confirmed_incidence_num"),
                           start_day = animalia::grab_start_day(aheads, 28, 14, "epiweek")(forecast_date),
                           geo_type = "state"),
  zebra = dplyr::tibble(data_source = c("jhu-csse",
                                        ## "usa-facts",
                                        "fb-survey",
                                        "doctor-visits"),
                        signal = c("confirmed_incidence_num",
                                   ## "deaths_incidence_num",
                                   "smoothed_hh_cmnty_cli",
                                   "smoothed_cli"),
                        start_day = animalia::grab_start_day(aheads, 28, 28, "epiweek")(forecast_date),
                        geo_type = "county")
)

## No changes below
anteater_signals  <- forecaster_signals[["anteater"]]
zebra_signals  <- forecaster_signals[["zebra"]]
