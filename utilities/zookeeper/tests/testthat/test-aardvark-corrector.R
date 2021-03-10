test_that("aardvark state corrector works", {
  aad <- make_aardvark_corrector()

  sigs <- suppressMessages(covidcast::covidcast_signals(
    "jhu-csse",
    c("deaths_incidence_num", "confirmed_incidence_num"),
    "2020-11-01",
    "2021-02-15",
    "state"
  ))
  corrected <- aad(sigs)
})
