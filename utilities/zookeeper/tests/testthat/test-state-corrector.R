library(mockery)

testthat::test_that("state corrector works", {
  ## Deterministic fakes/stubs for some functions that could be used to test
  ## output behavior; some aren't currently used here but might be useful if
  ## expanding test suite.
  rbinom_fake_all_heads <- function(n, size, prob) rep(size, n)
  rbinom_fake_all_tails <- function(n, size, prob) rep(0L, n)
  multinom_fake_all_first_choice <- function(n, size, prob) `[<-`(matrix(0L, length(prob),n), 1L,seq_len(n), size)
  multinom_fake_all_last_choice <- function(n, size, prob) `[<-`(matrix(0L, length(prob),n), length(prob),seq_len(n), size)
  ## `mockery::mock()`s record inputs but output simple input-independent stubs;
  ## combine with the input-dependent fakes/stubs to get mock objects that
  ## record inputs and produce outputs within the right range, with the right
  ## length/dimensions:
  rbinom_input_recorder <- mock()
  rbinom_full_mock <- function(...) {rbinom_input_recorder(...); rbinom_fake_all_tails(...)}
  rmultinom_input_recorder <- mock()
  rmultinom_full_mock <- function(...) {rmultinom_input_recorder(...); multinom_fake_all_first_choice(...)}
  mockr::with_mock(rbinom_wrapper = rbinom_full_mock, rmultinom_wrapper = rmultinom_full_mock, {
    start_date = as.Date("2020-11-01")
    end_date = as.Date("2021-02-15")
    aad <- make_state_corrector(
      manual_flags = tibble::tribble(
        ~data_source, ~signal, ~geo_value, ~time_value, ~max_lag,
        "jhu-csse", "deaths_incidence_num", "ak", end_date, 20L
      )
    )
    dummy_signal_precursor =
      tidyr::crossing(time_value=seq(start_date, end_date, 1L),
                      geo_value=sort(c(tolower(state.abb),"as","dc","gu","mp","pr","vi"))) %>%
      transmute(geo_value,
                time_value,
                ## lazy approach to dummy issue and lag: just put in extreme values to indicate laziness
                issue=as.Date("4000-01-01"),
                lag=as.integer(issue - time_value),
                ## just some constant dummy codes&values for everything else;
                ## these can later be `mutate`d into any more interesting
                ## configurations for testing:
                missing_value=0L,
                missing_stderr=0L,
                missing_sample_size=0L,
                value = -1,
                stderr=NA,
                sample_size=NA)
    ## pretend that toy data is jhu-csse death and case data:
    sigs = list(
      covidcast::as.covidcast_signal(
        as.data.frame(dummy_signal_precursor),
        data_source = "jhu-csse",
        signal = "deaths_incidence_num",
        geo_type = "state",
        time_type = "day"
      ),
      covidcast::as.covidcast_signal(
        as.data.frame(dummy_signal_precursor),
        data_source = "jhu-csse",
        signal = "confirmed_incidence_num",
        geo_type = "state",
        time_type = "day"
      )
    )
    ## First call to rmultinom_wrapper currently deals with the "ak" death data manual flag
    sigs[[1L]][["value"]][sigs[[1L]][["geo_value"]]=="ak"] <-
      rev(seq_len(sum(sigs[[1L]][["geo_value"]]=="ak")))
    ##
    corrected <- aad(sigs)
  })
  ##
  window_size = dplyr::filter(default_state_params(), .data$data_source=="jhu-csse", .data$signal=="deaths_incidence_num")[["window_size"]]
  if (window_size %% 2L != 0L) {
    rlang::abort("This test requires the default state corrector window size to be even; it will need to be updated if it is odd instead.", "zookeeper__test_state_corrector__window_size_not_even", window_size=window_size)
  }
  testthat::expect_equal(window_size %% 2L, 0L)
  ak_death_rbinom_call_args <- mock_args(rbinom_input_recorder)[[1L]]
  ## the excess will start with window_size-1 NA entries, and the remaining
  ## entries will all be (1-median(window_size:1)) = (window_size-1)/2; the
  ## former will be not be marked as bad excess and the latter will
  n_bad_excess = as.integer(end_date-start_date)+1L - (window_size-1L)
  ## (Using expect_equal not expect_identical in some places in order to allow
  ## int or dbl representations of int values)
  testthat::expect_equal(ak_death_rbinom_call_args[[1L]], n_bad_excess)
  testthat::expect_equal(ak_death_rbinom_call_args[[2L]], 1L)
  ## XXX not sure why this is 0.5 rather than excess[bad_excess] - floor(excess[bad_excess])
  testthat::expect_equal(ak_death_rbinom_call_args[[3L]], 0.5)
  ##
  ak_death_rmultinom_call_args <- mock_args(rmultinom_input_recorder)[[1L]]
  testthat::expect_equal(ak_death_rmultinom_call_args[[1L]], 1L)
  ## rbinom randomly fixes up bias when taking excess floor; in this test we
  ## replace the randomness with "all tails" (all 0s):
  rbinom_floor_bias_fixup_draw = 0L
  testthat::expect_identical(ak_death_rmultinom_call_args[[2L]],
                             ## "excess" in this example is floor(1 - median(<window size>:1)) + <rbinom floor-fixup>
                             window_size %/% 2L + rbinom_floor_bias_fixup_draw)
  ## The backdistribution here is controlled by the manual flag set up above
  testthat::expect_identical(ak_death_rmultinom_call_args[[3L]], rep(1/20, 20L))
})


