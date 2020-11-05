## code to prepare `state/correction_pars` dataset(s) goes here
##
correction_pars = list(
  window_size = 14,
  backfill_lag = 30,
  outlier_start_date = "2020-03-01",
  excess_cut = 0,
  size_cut = 20,
  sig_cut = 3,
  sig_consec = 2.25,
  max_county_cases = 10, #we require at least this
  min_county_reports = 30,
  total_counties = 300
)

usethis::use_data(correction_pars, overwrite = TRUE)
