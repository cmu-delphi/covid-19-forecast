# Files for current pipeline

## Corrections

1. In `county_corrections.Rmd` and `state_corrections.Rmd`, there is a parameter `write_RDS`. This is set to `FALSE`. I usually run the files and look at the html before deciding if I need to make adjustments. Once I'm done, I flip to `TRUE`. 
1. Run `county_corrections.Rmd`. Possibly with `rmarkdown::render("county_corrections.Rmd", params=list(write_RDS=TRUE))`.
1. Run `state_corrections.Rmd`. Possibly with `rmarkdown::render("state_corrections.Rmd", params=list(write_RDS=TRUE))`.

__Dependencies:__  
* A variety of packages
* `process_funs.R`
* Your `corrections.R` script
* The database

## Upstream df

## Run forecasters

## QA plots

Similar to corrections, should be able to run `run_checks_aardvark_state_death.Rmd` and `run_checks_zyzzyva_county_cases.Rmd`. Both take parameters (default values listed are for states):
```
upstream_df_date: "2020-11-01"
forecast_date: "2020-11-02"
response: "jhu-csse_deaths_incidence_num"
geo: "state"
upstream_data_path: "~/Documents/COVID-Delphi"
forecast_dir_path: "."
```

__Dependencies:__  
* A variety of packages
* `trajectories.R`, `anamolies.R`, `dists.R`

Note: These are a bit ugly (one overwrites functions in another, etc.) I got both to work on my maching, but my confidence is not high.

## Reichlab formatting

Here, just a script (produces 1 csv file named "XXXX-XX-XX-CMU-TimeSeries.csv" where the X's are the forecast date.

The top of the file has the following parameters:
```
path_to_production_run_output <- "/Users/dajmcdon/Documents/COVID-Delphi/covidcast-forecast/forecaster_predictions"
data_date <- lubridate::ymd("2020-11-02")
aheads <- 4:1
response_state <- "usa-facts_deaths_incidence_num"
response_cty <- "usa-facts_confirmed_incidence_num"
incidence_period <- "epiweek"
n_locations <- 200
```
Should only have to pass in the path to the output and the date of the data (I think this is the same as forecast date, not upstream_df_date).

__Dependencies:__
* A variety of packages
* `format.R`
