## Start of prologue
## These are usually not changed except by those know what is going
## on.  Volatile packages are those that undergo frequent development
## and are therefore installed just-in-time from repos before running
## forecasts

## Install a package if not already installed from github
install_from_github_if_needed <- function(pkgs, ...) {
  installed_pkgs <- installed.packages()[, 1]
  pkg_names <- names(pkgs)
  to_install <- pkgs[setdiff(pkg_names, installed_pkgs)]
  for (pkg in to_install) {
    devtools::install_github(repo = pkg$repo, ref = pkg$ref,
                             subdir = pkg$subdir, ...)
  }
}

volatile_pkgs <- list(
  evalcast = list(repo = "cmu-delphi/covidcast", ref = "evalcast",
                   subdir = "R-packages/evalcast"),
  modeltools = list(repo = "cmu-delphi/covidcast", ref = "modeltools",
                  subdir = "R-packages/modeltools"),
  zookeeper = list(repo = "cmu-delphi/covid-19-forecast", ref = "develop",
                   subdir = "utilities/zookeeper"),
  animalia = list(repo = "cmu-delphi/covid-19-forecast", ref = "develop",
                  subdir = "forecasters/animalia")
)

install_from_github_if_needed(volatile_pkgs, upgrade = "never")

`%magrittr>%` <- magrittr::`%>%`

## End of prologue

## Parameters section proper

## Obtain environment variables One place for all parameters used by
## all production scripts (production_script.R) and also markdowns
## (anteater.Rmd zebra.Rmd state-corrections.Rmd
## county-corrections.Rmd)

forecast_date <- lubridate::ymd(Sys.getenv("FORECAST_DATE"))
today  <- lubridate::ymd(Sys.getenv("TODAY"))

##output_dir  <- Sys.getenv("OUTPUT_DIR")
## We can fix this at /mnt
output_dir  <- "/mnt"

aheads  <- 1:4
qa_lookback <- 60 # how far back do we show actual data on the QA report?
correction_lookback <- 90 # how far back do we look on the daily corrections report?
n_counties <- 200 # we predict the top 200 counties


## Subdirectories for states and counties

state_output_subdir  <- "state-output"
county_output_subdir <- "county-output"

## Here is where you change the signals if one or the other is not available,
## or even add other replacement forecasters
forecaster_details  <- list(

  anteater = list(
    signals = dplyr::tibble( # state death forecaster
      data_source = "jhu-csse",
      signal = c("deaths_incidence_num",
                 "confirmed_incidence_num"
                 ),
      start_day = lubridate::ymd("2020-06-01"),
      geo_type = "state"
    ),
    ## The corresponding QC markdown file
    qc_markdown = "anteater.Rmd"
  ),

  zebra = list(
    signals = dplyr::tibble( # county case forecaster
      data_source = c("jhu-csse",
                      ## "usa-facts",
                      "fb-survey",
                      "doctor-visits"
                      ),
      signal = c("confirmed_incidence_num", "smoothed_hh_cmnty_cli", "smoothed_cli"),
      start_day = lubridate::ymd("2020-06-01"),
      geo_type = "county"
    ),
    ## The corresponding QC markdown file
    qc_markdown = "zebra.Rmd"
  )
)

state_corrections_md <- "state-corrections.Rmd"
county_corrections_md <- "county-corrections.Rmd"


## These should ideally be grouped

## Here is where you choose the forecasters you use for state and counties
state_forecaster_name  <- "anteater"
county_forecaster_name  <- "zebra"

## Start of DO NOT MODIFY: the next two lines are a consequence of the previous two lines
state_forecaster_signals  <- forecaster_details[[state_forecaster_name]][["signals"]]
county_forecaster_signals  <- forecaster_details[[county_forecaster_name]][["signals"]]

if (is.null(state_forecaster_signals) || is.null(county_forecaster_signals)) {
  stop("Forecaster details misconfigured! Please fix and rerun")
}
## End of DO NOT MODIFY

# State specific sets -----------------------------------------------------

state_corrections_params <- zookeeper::default_state_params(
  # many other options, see the function documentation
  data_source = state_forecaster_signals$data_source,
  signal = state_forecaster_signals$signal,
  geo_type = state_forecaster_signals$geo_type)

state_corrector <- zookeeper::make_state_corrector(
  params = state_corrections_params,
  # data, locations, times to do special correction processing
  manual_flags = tibble::tibble(
    data_source = "jhu-csse",
    signal = c(rep("deaths_incidence_num", 3),
               "confirmed_incidence_num",
               ## from JHU-CSSE notes 2021-04-17, 2021-04-18, 2021-04-24, 2021-04-25, 2021-05-16
               "deaths_incidence_num",
               "deaths_incidence_num",
               "confirmed_incidence_num",
               "confirmed_incidence_num",
               ## from JHU-CSSE notes 2021-05-02
               "deaths_incidence_num",
               ## from JHU-CSSE notes, https://covid19.nj.gov/faqs/announcements/all-announcements/covid-19-data-cleaning-update, https://www.nbcphiladelphia.com/news/coronavirus/new-jersey-coronavirus-2021-phil-murphy/2654709/
               "confirmed_incidence_num",
               "deaths_incidence_num",
               ## from spot checks
               "confirmed_incidence_num",
               ## KS state wday reporting behavior triggering inconsistent flagging; adjust some automatic flags
               "confirmed_incidence_num",
               ## from JHU-CSSE notes and spot checks for 2021-05-31
               ##   MD state deaths 2021-05-27
               "deaths_incidence_num",
               ##   NM state deaths 2021-05-24
               "deaths_incidence_num",
               ##   MO weekly (not completely regular) death cert review; starting to flag from Jan 5 but looks like goes back even further, mixed with other reporting events
               "deaths_incidence_num",
               ## new rows for 2021-06-07 ( + potential updates above and extra Memorial Day handling below)
               "confirmed_incidence_num",
               ## new rows for 2021-06-14
               ##   WA state deaths 2021-06-08, 2021-06-23?
               "deaths_incidence_num",
               ##   WI state deaths 2021-05-27
               "deaths_incidence_num",
               ## new rows for 2021-06-28
               ##   WA state cases 2021-06-23
               "confirmed_incidence_num",
               ## new rows for 2021-07-12
               ##   NJ state deaths 2021-06-09: "remove" auto-flag
               "deaths_incidence_num",
               ##   OH state deaths 2021-07-07, 2021-07-09: backdistribute as JHU-CSSE backdistribution wasn't available at time that covidcast fetched
               "deaths_incidence_num",
               ## new rows for 2021-07-19
               ##   FL weekly reporting: backdistribute each over a week
               "confirmed_incidence_num",
               "deaths_incidence_num",
               ##   IA weekly reporting
               "confirmed_incidence_num",
               "deaths_incidence_num"
               ),
    geo_value = c("va","ky","ok","ok",
                  ## from JHU-CSSE notes 2021-04-17, 2021-04-18, 2021-04-24, 2021-04-25, 2021-05-16
                  "ak","mi","mo","al",
                  ## from JHU-CSSE notes 2021-05-02
                  "wv",
                  ## from JHU-CSSE notes, https://covid19.nj.gov/faqs/announcements/all-announcements/covid-19-data-cleaning-update, https://www.nbcphiladelphia.com/news/coronavirus/new-jersey-coronavirus-2021-phil-murphy/2654709/
                  "nj",
                  "mt",
                  ## from spot checks
                  "hi",
                  ## KS state wday reporting behavior triggering inconsistent flagging; adjust some automatic flags
                  "ks",
                  ## from JHU-CSSE notes and spot checks for 2021-05-31
                  ##   MD state deaths 2021-05-27
                  "md",
                  ##   NM state deaths 2021-05-24
                  "nm",
                  ##   MO weekly (not completely regular) death cert review; starting to flag from Jan 5 but looks like goes back even further, mixed with other reporting events
                  "mo",
                  ## new rows for 2021-06-07 ( + potential updates above and extra Memorial Day handling below)
                  "in",
                  ## new rows for 2021-06-14
                  ##   WA state deaths 2021-06-08, 2021-06-23?
                  "wa",
                  ##   WI state deaths 2021-05-27
                  "wi",
                  ## new rows for 2021-06-28
                  ##   WA state cases 2021-06-23
                  "wa",
                  ## new rows for 2021-07-12
                  ##   NJ state deaths 2021-06-09: "remove" auto-flag
                  "nj",
                  ##   OH state deaths 2021-07-13, 2021-07-16: backdistribute as JHU-CSSE backdistribution wasn't available at time that covidcast fetched
                  "oh",
                  ## new rows for 2021-07-19
                  ##   FL weekly reporting: backdistribute each over a week
                  "fl",
                  "fl",
                  ##   IA weekly reporting
                  "ia",
                  "ia"
                  ),
    time_value = list(
      seq(lubridate::ymd("2021-02-21"), lubridate::ymd("2021-03-04"), by = 1),
      lubridate::ymd(c("2021-03-18","2021-03-19","2021-06-01")),
      lubridate::ymd(c("2021-04-07","2021-05-26")),
      lubridate::ymd("2021-04-07"),
      ## from JHU-CSSE notes 2021-04-17, 2021-04-18, 2021-04-24, 2021-04-25, 2021-05-16
      lubridate::ymd("2021-04-15"),
      lubridate::ymd(c("2021-04-01", "2021-04-03", "2021-04-06", "2021-04-08", "2021-04-10", "2021-04-13", "2021-04-15", "2021-04-17",
                       "2021-04-20", "2021-04-22", "2021-04-24",
                       "2021-04-27", "2021-04-29", "2021-05-01",
                       "2021-05-04", "2021-05-06", "2021-05-08",
                       "2021-05-11", "2021-05-13", "2021-05-15",
                       "2021-05-18", "2021-05-20", "2021-05-22",
                       "2021-05-25", "2021-05-27", "2021-05-29",
                       ## "2021-05-31", # Memorial Day
                       "2021-06-01", "2021-06-03", "2021-06-05",
                       "2021-06-08", "2021-06-10", "2021-06-12",
                       "2021-06-15", "2021-06-17", # "2021-06-19", # zero on Juneteenth
                       ## "2021-06-21", # assume that this is weekend backfill excluding any death cert review
                       "2021-06-22", "2021-06-24", # ,  "2021-06-26", # zero on this Saturday as well; maybe stopped reporting on Saturdays?
                       "2021-06-29", "2021-07-01" # seems like stopped reporting on Saturdays.
                       ## week of 2021-07-06 seems to be point where deaths switched to Tue&Fri reporting; not sure if the death cert reviews are separated; just stop flagging (decision made 2021-07-19)
                       )),
      lubridate::ymd("2021-04-17","2021-06-02"),
      lubridate::ymd("2021-04-13","2021-04-20","2021-05-13","2021-05-14","2021-05-15"), # (2021-05-15 seems along the lines of the two preceding anomalous days)
      ## from JHU-CSSE notes 2021-05-02
      lubridate::ymd("2021-04-27"),
      ## from JHU-CSSE notes, https://covid19.nj.gov/faqs/announcements/all-announcements/covid-19-data-cleaning-update, https://www.nbcphiladelphia.com/news/coronavirus/new-jersey-coronavirus-2021-phil-murphy/2654709/
      lubridate::ymd(c("2021-04-26",
                       "2021-05-05", "2021-05-06")),
      lubridate::ymd("2021-05-07"),
      ## from spot checks
      lubridate::ymd(c("2021-03-12","2021-03-13", "2021-03-19", "2021-04-02")),
      ## KS state wday reporting behavior triggering inconsistent flagging; adjust some automatic flags
      lubridate::ymd(c("2021-04-19","2021-04-26","2021-05-03","2021-05-10")),
      ## from JHU-CSSE notes and spot checks for 2021-05-31
      ##   MD state deaths 2021-05-27
      lubridate::ymd(c("2021-05-27")),
      ##   NM state deaths 2021-05-24
      lubridate::ymd(c("2021-05-24")),
      ##   MO weekly (not completely regular) death cert review; starting to flag from Jan 5 but looks like goes back even further, mixed with other reporting events; a couple of spikes not flagged as they are close to larger spikes and might not be death cert review
      lubridate::ymd(c("2021-01-05","2021-01-12","2021-01-20","2021-01-26","2021-02-03","2021-02-11","2021-02-18","2021-02-23","2021-03-03","2021-03-09","2021-03-16","2021-03-23","2021-03-30","2021-04-13","2021-04-20","2021-04-27","2021-05-04","2021-05-11","2021-05-18","2021-05-25","2021-06-02","2021-06-08","2021-06-15","2021-06-22","2021-06-29","2021-07-07","2021-07-13")),
      ## new rows for 2021-06-07 ( + potential updates above and extra Memorial Day handling below)
      lubridate::ymd("2021-06-03"),
      ## new rows for 2021-06-14
      ##   WA state deaths 2021-06-08, 2021-06-23?
      lubridate::ymd(c("2021-06-08", "2021-06-23")),
      ##   WI state deaths 2021-05-27
      lubridate::ymd("2021-05-27"),
      ## new rows for 2021-06-28
      ##   WA state cases 2021-06-23
      lubridate::ymd("2021-06-23"),
      ## new rows for 2021-07-12
      ##   NJ state deaths 2021-06-09: "remove" auto-flag
      lubridate::ymd("2021-06-09"),
      ##   OH state deaths 2021-07-13, 2021-07-16: backdistribute as JHU-CSSE backdistribution wasn't available at time that covidcast fetched
      lubridate::ymd(c("2021-07-13","2021-07-16")),
      ## new rows for 2021-07-19
      ##   FL weekly reporting: backdistribute each over a week
      lubridate::ymd(c("2021-06-11","2021-06-18","2021-06-25","2021-07-02","2021-07-09","2021-07-16")),
      lubridate::ymd(c("2021-06-11","2021-06-18","2021-06-25","2021-07-02","2021-07-09","2021-07-16")),
      ##   IA weekly reporting
      lubridate::ymd(c("2021-07-14")),
      lubridate::ymd(c("2021-07-14"))
      ),
    max_lag = c(rep(90, 4),
                ## from JHU-CSSE notes 2021-04-17, 2021-04-18, 2021-04-24, 2021-04-25, 2021-05-16
                75, 150, 150, 180, # (AL 2021-05-13,2021-05-14,2021-05-15 would be a couple months larger if different max_lag's allowed, plus would use min_lag if available)
                ## from JHU-CSSE notes 2021-05-02; just assign an arbitrary large value due to lack of accessible details
                180,
                ## from JHU-CSSE notes, https://covid19.nj.gov/faqs/announcements/all-announcements/covid-19-data-cleaning-update, https://www.nbcphiladelphia.com/news/coronavirus/new-jersey-coronavirus-2021-phil-murphy/2654709/
                121, # (the 2021-04-26 duplicate removal should probably go back further, say 400 instead of 121, when required feature is implemented)
                218, # (if implement corresponding min_lag, would set its value to 96)
                ## from spot checks
                1+1, # not sure of correct value, but having last spike up drop down a lot doesn't seem right
                ## KS state wday reporting behavior triggering inconsistent flagging; adjust some automatic flags
                2+1, # not sure of correct value; intending to spread across date itself + 2 preceding days
                ## from JHU-CSSE notes and spot checks for 2021-05-31
                ##   MD state deaths 2021-05-27
                180, # somewhat arbitrary; not taken from an announcement
                ##   NM state deaths 2021-05-24
                180, # somewhat arbitrary; not taken from an announcement
                ##   MO weekly (not completely regular) death cert review; starting to flag from Jan 5 but looks like goes back even further, mixed with other reporting events
                180,
                ## new rows for 2021-06-07 ( + potential updates above and extra Memorial Day handling below)
                180, # arbitrary
                ## new rows for 2021-06-14
                ##   WA state deaths 2021-06-08
                120,
                ##   WI state deaths 2021-05-27
                60,
                ## new rows for 2021-06-28
                ##   WA state cases 2021-06-23
                120,
                ## new rows for 2021-07-12
                ##   NJ state deaths 2021-06-09: "remove" auto-flag
                1,
                ##   OH state deaths 2021-07-13, 2021-07-16: backdistribute as JHU-CSSE backdistribution wasn't available at time that covidcast fetched
                21,
                ## new rows for 2021-07-19
                ##   FL weekly reporting: backdistribute each over a week
                7,
                7,
                ##   IA weekly reporting
                7,
                7
                )
    ) %magrittr>%
    ## {
    ##   previous <- .
    ##   geo_values_with_memorial_day_response_zeros_unlike_previous_monday <- c("co", "ct", "dc", "fl", "id", "in", "ks", "ky", "la", "ma",  "me", "mi", "nc", "ne", "nm", "nv", "oh", "ri", "tn", "wa", "wv" )
    ##   time_values_to_flag_for_memorial_day <- lubridate::ymd(c("2021-05-31","2021-06-01"))
    ##   previous %>%
    ##     dplyr::mutate(time_value = dplyr::if_else(geo_value %in% geo_values_with_memorial_day_response_zeros_unlike_previous_monday,
    ##                                               lapply(time_value, function(time_value_elt) c(time_value_elt, lubridate::as_date(setdiff(time_values_to_flag_for_memorial_day, time_value_elt)))),
    ##                                               as.list(time_value))) %>%
    ##     dplyr::bind_rows(
    ##              tidyr::crossing(data_source="jhu-csse",
    ##                              signal=c("confirmed_incidence_num","deaths_incidence_num"),
    ##                              geo_value=geo_values_with_memorial_day_response_zeros_unlike_previous_monday) %>%
    ##              setdiff(previous[c("data_source","signal","geo_value")]) %>%
    ##              dplyr::mutate(time_value = list(time_values_to_flag_for_memorial_day),
    ##                            max_lag = 180 # arbitrary large value to try to make this like just filling in with smoothed values
    ##                            )
    ##            )
    ## }
    identity()
)

state_forecaster_args <- list(
  ahead = aheads,
  lags = c(0,7,14),
  tau = evalcast::covidhub_probs(), # 23 quantiles
  lambda = 0, # no regularization or CV
  lp_solver = "gurobi", # can remove if no license
  noncross = TRUE, # takes a bit longer, but not much
  featurize = animalia::make_7dav_featurizer(), # has no arguments
  verbose = TRUE,
  signals_to_normalize = c(TRUE, TRUE),
  save_wide_data = file.path(output_dir, state_output_subdir),
  save_trained_models = file.path(output_dir, state_output_subdir)
)


# County specific sets ----------------------------------------------------

county_corrections_params  <- zookeeper::default_county_params(
  data_source = county_forecaster_signals$data_source,
  signal = county_forecaster_signals$signal[1] # only correct cases
)

county_corrector  <- zookeeper::make_county_corrector(
  params = county_corrections_params,
  manual_flags = tibble::tibble(
    data_source = "jhu-csse",
    signal = "confirmed_incidence_num",
    geo_value = c(
      ## from JHU-CSSE notes 2021-04-17, 2021-04-18, 2021-04-24, 2021-04-25, 2021-05-16
      "29077", "29095", "29183", "29189",
      c("01000", "01001", "01003", "01005", "01007", "01009", "01011",  "01013", "01015", "01017", "01019", "01021", "01023", "01025",  "01027", "01029", "01031", "01033", "01035", "01037", "01039",  "01041", "01043", "01045", "01047", "01049", "01051", "01053",  "01055", "01057", "01059", "01061", "01063", "01065", "01067",  "01069", "01071", "01073", "01075", "01077", "01079", "01081",  "01083", "01085", "01087", "01089", "01091", "01093", "01095",  "01097", "01099", "01101", "01103", "01105", "01107", "01109",  "01111", "01113", "01115", "01117", "01119", "01121", "01123",  "01125", "01127", "01129", "01131", "01133"),
      ## from JHU-CSSE notes, https://covid19.nj.gov/faqs/announcements/all-announcements/covid-19-data-cleaning-update, https://www.nbcphiladelphia.com/news/coronavirus/new-jersey-coronavirus-2021-phil-murphy/2654709/
      "34003",
      c("34001", "34005", "34007", "34009", "34011", "34013", "34015", "34017", "34019", "34021", "34023", "34025", "34027", "34029", "34031", "34033", "34035", "34037", "34039", "34041"),
      ## from spot checks
      "06095",
      ## Tulare CA inconsistent flagging of recent spikes; try backdistributing unflagged spikes (ongoing issues as of 2021-06-14)
      "06107",
      ## Philadelphia PA apparent schedule change triggers inappropriate flags
      "42101",
      ## from JHU-CSSE notes and spot checks for 2021-05-31
      ##   Los Angeles CA cases 2021-05-27
      "06037",
      ##   King WA, Pierce WA, Spokane WA cases 2021-05-{14-17,21-23,24,26,30,31}, 2021-06-{07,09}
      c("53033","53053","53063"),
      ## new rows for 2021-06-07 ( + potential updates above and extra Memorial Day handling below)
      ##   Pima AZ
      "04019",
      ##   Forsyth NC
      "37067",
      ##   Horry SC
      "45051",
      ##   Harris TX
      "48201",
      ##   Bexar TX
      "48029",
      ## new rows for 2021-06-14
      ##   Webb TX
      "48479",
      ## new rows for 2021-06-28
      ##   Jefferson TX
      "48245",
      ##   Collin TX better backdistribution shape
      "48085",
      ## new rows for 2021-07-05
      ##   Fresno CA 2021-07-02 spike down backdistribute longer
      "06019",
      ##   San Francisco CA 2021-06-30 spike down backdistribute longer
      "06075",
      ##   San Mateo CA 2021-06-30 spike down backdistribute longer
      "06081",
      ##   Santa Clara CA 2021-06-30 spike down backdistribute longer
      "06085",
      ##   Butler OH 2021-07-01 spike up backdistribute longer
      "39017",
      ## new rows for 2021-07-12
      ##   Orange CA cases 2021-07-06: backdistribute shorter
      "06059",
      ##   Riverside CA cases 2021-07-08: backdistribute longer
      "06065",
      ##   Sacramento CA cases 2021-06-19, 2021-07-06: backdistribute shorter
      "06067",
      ##   San Diego CA cases 2021-07-07: backdistribute longer (there is a preceding reporting day, so maybe this isn't preceding 0 days in batch but a longer period)
      "06073",
      ##   Fulton GA cases 2021-07-06: backdistribute shorter
      "13121",
      ##   Polk IA cases 2021-07-07: backdistribute longer
      "19153",
      ##   Jefferson LA cases 2021-07-06: backdistribute shorter
      "22051",
      ##   Williamson TN cases 2021-04-19, 2021-06-11: backdistribute longer
      "47187",
      ##   Brazos TX cases 2021-05-24: backdistribute longer
      "48041",
      ##   Galveston TX cases 2021-07-07: backdistribute shorter (why are some of these three zeros followed by data happening on 2021-07-06 and others on 2021-07-07?)
      "48167",
      ##   McLennan TX cases 2021-07-07, 2021-07-10: backdistribute longer
      "48309",
      ##   Travis TX cases 2021-07-06: backdistribute shorter
      "48453",
      ## new rows for 2021-07-12
      ##   FL weekly reporting: backdistribute each over a week
      c("12001", "12003", "12005", "12007", "12009", "12011", "12013",  "12015", "12017", "12019", "12021", "12023", "12027", "12029",  "12031", "12033", "12035", "12037", "12039", "12041", "12043",  "12045", "12047", "12049", "12051", "12053", "12055", "12057",  "12059", "12061", "12063", "12065", "12067", "12069", "12071",  "12073", "12075", "12077", "12079", "12081", "12083", "12085",  "12086", "12087", "12089", "12091", "12093", "12095", "12097",  "12099", "12101", "12103", "12105", "12107", "12109", "12111",  "12113", "12115", "12117", "12119", "12121", "12123", "12125",  "12127", "12129", "12131", "12133")
    ),
    time_value = c(
      list(
        ## from JHU-CSSE notes 2021-04-17, 2021-04-18, 2021-04-24, 2021-04-25, 2021-05-16
        lubridate::ymd(c("2021-03-11","2021-04-17")), lubridate::ymd(c("2021-03-11","2021-04-17")), lubridate::ymd(c("2021-03-11","2021-04-17")),
        lubridate::ymd("2021-04-17")
        ),
      rep(list(
        lubridate::ymd("2021-04-13",
                       "2021-04-20",
                       "2021-05-13", "2021-05-14", "2021-05-15", # (2021-05-15 seems along the lines of the two preceding anomalous days)
                       "2021-07-09") # 2021-07-09 sticks out especially in Mobile AL but also in others; might be a similar statewide effect
      ), 68L),
      ## from JHU-CSSE notes, https://covid19.nj.gov/faqs/announcements/all-announcements/covid-19-data-cleaning-update, https://www.nbcphiladelphia.com/news/coronavirus/new-jersey-coronavirus-2021-phil-murphy/2654709/
      list(lubridate::ymd(c("2021-04-26",
                            ## (Bergen NJ antigen case addition seem to be 2 days rather than just 2021-05-05?)
                            "2021-05-05", "2021-05-06"))),
      rep(list(lubridate::ymd(c("2021-04-26"))), 21L-1L),
      ## from spot checks
      list(lubridate::ymd(c("2021-02-08","2021-04-26"))),
      ## Tulare CA inconsistent flagging of recent spikes; try backdistributing some unflagged spikes + redundantly tagging some to ensure (ongoing issues as of 2021-06-14)
      list(lubridate::ymd(c("2021-05-20", "2021-05-26","2021-05-27","2021-05-28","2021-06-01","2021-06-04","2021-06-07","2021-06-08","2021-06-12"))),
      ## Philadelphia PA apparent schedule change triggers inappropriate flags
      list(lubridate::ymd(c("2021-05-03","2021-05-06","2021-05-10","2021-05-13","2021-05-17","2021-05-20"))),
      ## from JHU-CSSE notes and spot checks for 2021-05-31
      ##   Los Angeles CA cases 2021-05-27
      list(lubridate::ymd("2021-05-27")),
      ##   King WA, Pierce WA, Spokane WA cases 2021-05-{14-17,21-23,24,26,30,31}, 2021-06-{07,09}
      rep(list(lubridate::ymd(c("2021-05-14","2021-05-15","2021-05-16","2021-05-17","2021-05-21","2021-05-22","2021-05-23","2021-05-24","2021-05-26",
                                "2021-05-30","2021-05-31",
                                "2021-06-07","2021-06-09"))), 3L),
      ## new rows for 2021-06-07 ( + potential updates above and extra Memorial Day handling below)
      ##   Pima AZ
      list(lubridate::ymd(c("2021-05-31","2021-06-05","2021-06-06"))),
      ##   Forsyth NC
      list(lubridate::ymd(c("2021-06-02","2021-06-03","2021-06-04"))),
      ##   Horry SC
      list(lubridate::ymd(c("2021-05-31","2021-06-01"))),
      ##   Harris TX
      list(lubridate::ymd(c("2021-05-31","2021-06-01","2021-06-02","2021-06-24"))),
      ##   Bexar TX
      list(lubridate::ymd(c("2021-06-03","2021-05-24","2021-05-17","2021-05-10","2021-05-03","2021-04-26","2021-04-19","2021-04-12","2021-04-05","2021-03-28","2021-03-21"))),
      ## new rows for 2021-06-14
      ##   Webb TX
      list(lubridate::ymd("2021-06-09")),
      ## new rows for 2021-06-28
      ##   Jefferson TX
      list(lubridate::ymd("2021-06-22")),
      ##   Collin TX better backdistribution shape
      list(lubridate::ymd("2021-06-12")),
      ## new rows for 2021-07-05
      ##   Fresno CA 2021-07-02 spike down backdistribute longer
      list(lubridate::ymd("2021-07-02")),
      ##   San Francisco CA 2021-06-30 spike down backdistribute longer
      list(lubridate::ymd("2021-06-30")),
      ##   San Mateo CA 2021-06-30 spike down backdistribute longer
      list(lubridate::ymd("2021-06-30")),
      ##   Santa Clara CA 2021-06-30 spike down backdistribute longer
      list(lubridate::ymd("2021-06-30")),
      ##   Butler OH 2021-07-01 spike up backdistribute longer
      list(lubridate::ymd("2021-07-01")),
      ## new rows for 2021-07-12
      ##   Orange CA cases 2021-07-06: backdistribute shorter
      list(lubridate::ymd("2021-07-06")),
      ##   Riverside CA cases 2021-07-08: backdistribute longer
      list(lubridate::ymd("2021-07-08")),
      ##   Sacramento CA cases 2021-06-19, 2021-07-06: backdistribute shorter
      list(lubridate::ymd(c("2021-06-19","2021-07-08"))),
      ##   San Diego CA cases 2021-07-07: backdistribute longer (there is a preceding reporting day, so maybe this isn't preceding 0 days in batch but a longer period)
      list(lubridate::ymd("2021-07-07")),
      ##   Fulton GA cases 2021-07-06: backdistribute shorter
      list(lubridate::ymd("2021-07-06")),
      ##   Polk IA cases 2021-07-07: backdistribute longer
      list(lubridate::ymd("2021-07-07")),
      ##   Jefferson LA cases 2021-07-06: backdistribute shorter
      list(lubridate::ymd("2021-07-06")),
      ##   Williamson TN cases 2021-04-19, 2021-06-11: backdistribute longer
      list(lubridate::ymd(c("2021-04-19","2021-06-11"))),
      ##   Brazos TX cases 2021-05-24: backdistribute longer
      list(lubridate::ymd("2021-05-24")),
      ##   Galveston TX cases 2021-07-07: backdistribute shorter (why are some of these three zeros followed by data happening on 2021-07-06 and others on 2021-07-07?)
      list(lubridate::ymd("2021-07-07")),
      ##   McLennan TX cases 2021-07-07, 2021-07-10: backdistribute longer
      list(lubridate::ymd(c("2021-07-07","2021-07-10"))),
      ##   Travis TX cases 2021-07-06: backdistribute shorter
      list(lubridate::ymd("2021-07-06")),
      ## new rows for 2021-07-12
      ##   FL weekly reporting: backdistribute each over a week
      rep(list(lubridate::ymd(c("2021-06-11","2021-06-18","2021-06-25","2021-07-02","2021-07-09","2021-07-16"))),
          67L)
    ),
    max_lag = c(
      ## from JHU-CSSE notes 2021-04-17, 2021-04-18, 2021-04-24, 2021-04-25, 2021-05-16
      rep(150, 4L),
      ## (`max_lag` could be selected better when able to have different values for different `time_value`s or with a `min_lag`)
      rep(180, 68L),
      ## from JHU-CSSE notes, https://covid19.nj.gov/faqs/announcements/all-announcements/covid-19-data-cleaning-update, https://www.nbcphiladelphia.com/news/coronavirus/new-jersey-coronavirus-2021-phil-murphy/2654709/
      121, # (the 2021-04-26 duplicate removal should probably go back further, say 400 instead of 121, when required feature is implemented)
      rep(400, 21L-1L),
      ## from spot checks
      3+1,
      ## Tulare CA inconsistent flagging of recent spikes; try backdistributing unflagged spikes (ongoing issues as of 2021-06-14)
      14,
      ## Philadelphia PA apparent schedule change triggers inappropriate flags
      3+1, # if max_lag allowed to vary here, should do 2+1 and then alternate between 2+1 and 3+1
      ## from JHU-CSSE notes and spot checks for 2021-05-31
      ##   Los Angeles CA cases 2021-05-27
      180, # somewhat arbitrary; not taken from an announcement
      ##   King WA, Pierce WA, Spokane WA cases 2021-05-{14-17,21-23,24,26,30,31}, 2021-06-{07,09}
      rep(180, 3L), # somewhat arbitrary; not taken from an announcement
      ## new rows for 2021-06-07 ( + potential updates above and extra Memorial Day handling below)
      ##   Pima AZ
      60,
      ##   Forsyth NC
      180, # try to approximate replacing with smoothed values
      ##   Horry SC
      180,
      ##   Harris TX
      180,
      ##   Bexar TX
      7,
      ## new rows for 2021-06-14
      ##   Webb TX
      180,
      ## new rows for 2021-06-28
      ##   Jefferson TX
      20, # for June 3 -- June 22 on June 22: 20 days
      ##   Collin TX better backdistribution shape
      15, # appears to be for 15 days from 2021-05-28 to 2021-06-12
      ## new rows for 2021-07-05
      ##   Fresno CA 2021-07-02 spike down backdistribute longer
      180,
      ##   San Francisco CA 2021-06-30 spike down backdistribute longer
      180,
      ##   San Mateo CA 2021-06-30 spike down backdistribute longer
      180,
      ##   Santa Clara CA 2021-06-30 spike down backdistribute longer
      180,
      ##   Butler OH 2021-07-01 spike up backdistribute longer
      180,
      ## new rows for 2021-07-12
      ##   Orange CA cases 2021-07-06: backdistribute shorter
      4,
      ##   Riverside CA cases 2021-07-08: backdistribute longer
      180,
      ##   Sacramento CA cases 2021-06-19, 2021-07-06: backdistribute shorter
      4,
      ##   San Diego CA cases 2021-07-07: backdistribute longer (there is a preceding reporting day, so maybe this isn't preceding 0 days in batch but a longer period)
      180,
      ##   Fulton GA cases 2021-07-06: backdistribute shorter
      4,
      ##   Polk IA cases 2021-07-07: backdistribute longer
      180,
      ##   Jefferson LA cases 2021-07-06: backdistribute shorter
      4,
      ##   Williamson TN cases 2021-04-19, 2021-06-11: backdistribute longer
      180,
      ##   Brazos TX cases 2021-05-24: backdistribute longer
      180,
      ##   Galveston TX cases 2021-07-07: backdistribute shorter (why are some of these three zeros followed by data happening on 2021-07-06 and others on 2021-07-07?)
      4,
      ##   McLennan TX cases 2021-07-07, 2021-07-10: backdistribute longer
      18, # days in range [2021-06-20..2021-07-07] (inclusive, based on current understanding of `max_lag` implementation)
      ##   Travis TX cases 2021-07-06: backdistribute shorter
      4,
      ## new rows for 2021-07-12
      ##   FL weekly reporting: backdistribute each over a week
      rep(7, 67L)
    )
  ) %magrittr>%
    ## {
    ##   previous <- .
    ##   geo_values_with_memorial_day_response_zeros_unlike_previous_monday <- c("01073", "01089", "01097", "06053", "06059", "06065", "06067",  "06071", "06077", "06095", "06099", "06107", "06111", "09001",  "09003", "09009", "11001", "12009", "12011", "12021", "12031",  "12033", "12057", "12071", "12081", "12086", "12095", "12097",  "12099", "12101", "12103", "12105", "12115", "12117", "12127",  "16001", "18003", "18057", "18089", "18097", "18141", "20091",  "20173", "21067", "21111", "22033", "22051", "25005", "25009",  "25013", "25017", "25021", "25023", "25025", "25027", "26049",  "26081", "26099", "26125", "26163", "27003", "27037", "27053",  "27123", "31055", "32003", "32031", "33011", "35001", "37067",  "37081", "37119", "37183", "39017", "39035", "39049", "39061",  "39095", "39113", "39153", "42101", "44007", "47037", "47065",  "47093", "47149", "47157", "48029", "48085", "48113", "48121",  "48157", "48339", "48439", "48491", "51153", "53033", "53053",  "53061", "53063")
    ##   time_values_to_flag_for_memorial_day <- lubridate::ymd(c("2021-05-31","2021-06-01"))
    ##   previous %>%
    ##     dplyr::mutate(time_value = dplyr::if_else(geo_value %in% geo_values_with_memorial_day_response_zeros_unlike_previous_monday,
    ##                                               lapply(time_value, function(time_value_elt) c(time_value_elt, lubridate::as_date(setdiff(time_values_to_flag_for_memorial_day, time_value_elt)))),
    ##                                               as.list(time_value))) %>%
    ##     dplyr::bind_rows(
    ##              tidyr::crossing(data_source="jhu-csse",
    ##                              signal="confirmed_incidence_num",
    ##                              geo_value=geo_values_with_memorial_day_response_zeros_unlike_previous_monday) %>%
    ##              setdiff(previous[c("data_source","signal","geo_value")]) %>%
    ##              dplyr::mutate(time_value = list(time_values_to_flag_for_memorial_day),
    ##                            max_lag = 180 # arbitrary large value to try to make this like just filling in with smoothed values
    ##                            )
    ##            )
    ## }
    identity()
)

prob_type <- ifelse(county_forecaster_signals$signal[1] == "confirmed_incidence_num",
                    "inc_case", "standard")

county_forecaster_args <- list(
  ahead = aheads,
  lags = list(c(0, 1, 2, seq(3, 21, 3)), seq(3,28,7), seq(3,28,7)),
  tau = evalcast::covidhub_probs(type = prob_type), # only 7 quantiles for inc_cases
  lambda = 0,
  lp_solver = "gurobi",
  noncross = TRUE,
  geo_value_selector = animalia::select_geo_top_n(
    n_locations = n_counties
  ),
  featurize = animalia::make_7dav_featurizer(),
  #signals_to_normalize = c(TRUE, FALSE, FALSE), 
  verbose = TRUE,
  save_wide_data = file.path(output_dir, county_output_subdir),
  save_trained_models = file.path(output_dir, county_output_subdir)
)

