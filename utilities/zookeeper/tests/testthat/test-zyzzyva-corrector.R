test_that("zyzzyva county corrector works", {
  sigs <- suppressMessages(covidcast::covidcast_signals(
    c("usa-facts","fb-survey"),
    c("confirmed_incidence_num", "smoothed_hh_cmnty_cli"),
    "2021-01-01",
    "2021-02-15",
    "county"
  ))
  zyz <- make_zyzzyva_corrector()
  corrected <- zyz(sigs)
})
