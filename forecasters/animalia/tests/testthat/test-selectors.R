df_county_wide <- tibble::tibble(
  geo_value = rep(c("00001", "00002", "00003", "00004", "00005"), each = 3),
  time_value = rep(as.Date("2021-01-01") + 0:2, times = 5),
  `value+0:jhu-csse_confirmed_incidence_num` = 1:15,
  `value-7:jhu-csse_confirmed_incidence_num` = 15:1,
  `value+7:jhu-csse_confirmed_incidence_num` = c(1:10, 1:5)
)

df_state_wide <- tibble::tibble(
  geo_value = rep(c("ak", "ca", "fl", "tx", "wa"), each = 3),
  time_value = rep(as.Date("2021-01-01") + 0:2, times = 5),
  `value+0:jhu-csse_deaths_incidence_num` = 1:15,
  `value-7:jhu-csse_deaths_incidence_num` = 15:1,
  `value+7:jhu-csse_deaths_incidence_num` = c(1:10, 1:5)
)

df_wide <- tibble::tibble(
  geo_value = rep(c("00001", "00002", "00003", "00004", "00005"), each = 3),
  time_value = rep(as.Date("2021-01-01") + 0:2, times = 5),
  `value+0:fb-survey_smoothed_cli` = 1:15,
  `value-7:fb-survey_smoothed_cli` = 15:1,
  `value+7:fb-survey_smoothed_cli` = c(1:10, 1:5)
)


test_that("select_geo_top_n, filter not active", {
  selection_fn <- select_geo_top_n()
  expect_equal(selection_fn(df_county_wide),
               c("00005", "00004", "00003", "00002", "00001"))
})

test_that("select_geo_top_n, filter is active", {
  selection_fn <- select_geo_top_n(n_locations = 2)
  expect_equal(selection_fn(df_county_wide),
               c("00005", "00004"))
})

test_that("select_geo_top_n, different response signal", {
  selection_fn <- select_geo_top_n(response_signal = "deaths_incidence_num",
                                     n_locations = 3)
  expect_equal(selection_fn(df_state_wide),
               c("wa", "tx", "fl"))
})

test_that("select_geo_top_n, different response data source", {
  selection_fn <- select_geo_top_n(response_data_source = "fb-survey",
                                   response_signal = "smoothed_cli",
                                   n_locations = 1)
  expect_equal(selection_fn(df_wide),
               c("00005"))
})

test_that("select_geo_top_n, invalid n_locations", {
  expect_error(select_geo_top_n(n_locations = 0))
})
