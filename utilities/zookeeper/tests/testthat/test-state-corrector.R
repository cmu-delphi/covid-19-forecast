testthat::test_that("state corrector works", {
  ## Fakes/stubs for some functions that could be used to test output behavior, but currently aren't.
  rbinom_min <- function(n, size, prob) rep(0L, n)
  rbinom_max <- function(n, size, prob) rep_along(size, n)
  rmultinom_first <- function(n, size, prob) `[<-`(matrix(0L, length(prob),n), 1L,seq_len(n), size)
  rmultinom_last <- function(n, size, prob) `[<-`(matrix(0L, length(prob),n), length(prob),seq_len(n), size)
  rmultinom_mock <- mockery::mock()
  rmultinom_fullmock <- function(...) {rmultinom_mock(...); rmultinom_first(...)}
  ##
  mockr::with_mock(rmultinom_wrapper = rmultinom_fullmock, {
    aad <- make_state_corrector(
      manual_flags = tibble::tribble(
                               ~data_source, ~signal, ~geo_value, ~time_value, ~max_lag,
                               "jhu-csse", "deaths_incidence_num", "ak", as.Date("2021-02-15"), 20L
                             )
    )
    ##
    sigs <- suppressMessages(covidcast::covidcast_signals(
                                          "jhu-csse",
                                          c("deaths_incidence_num", "confirmed_incidence_num"),
                                          "2020-11-01",
                                          "2021-02-15",
                                          "state"
                                        ))
    ## First call to rmultinom_wrapper currently deals with the "ak" death data manual flag
    sigs[[1L]][["value"]][sigs[[1L]][["geo_value"]]=="ak"] <-
      rev(seq_len(sum(sigs[[1L]][["geo_value"]]=="ak")))
    ##
    corrector_warnings <- testthat::capture_warnings({
      corrected <- aad(sigs)
    })
  }, .env=asNamespace("zookeeper")) # .env setting due to https://github.com/krlmlr/mockr/issues/7
  ##
  testthat::expect_match(corrector_warnings, "multinomial trick", all=TRUE)
  ak_death_call_args <- mockery::mock_args(rmultinom_mock)[[1L]]
  print(ak_death_call_args[[2L]])
  print(class(ak_death_call_args[[2L]]))
  testthat::expect_identical(ak_death_call_args[[1L]][[2L]],
                             ## rolling filter median on 1:<applicable window size>; appears to take min on this type of tie
                             dplyr::filter(default_state_params(), .data$data_source=="jhu-csse", .data$signal=="deaths_incidence_num")[["window_size"]] %/% 2L)
  testthat::expect_identical(ak_death_call_args[[3L]], rep(1/20, 20L))
})
