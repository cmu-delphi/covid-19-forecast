df_county_wide <- tibble::tibble(
  data_source = "jhu-csse",
  signal = "confirmed_incidence_num",
  geo_value = rep(c("00001", "00002", "00003", "00004", "00005"), each = 3),
  time_value = rep(as.Date("2021-01-01") + 0:2, times = 5),
  value = 1:15
)

df_state_wide <- list(
  tibble::tibble(
    data_source = "jhu-csse",
    signal = "csse_deaths_incidence_num",
    geo_value = rep(c("ak", "ca", "fl", "tx", "wa"), each = 3),
    time_value = rep(as.Date("2021-01-01") + 0:2, times = 5),
    value = c(1:3, 1:3, 2:4, c(2, 3, 5), c(6, 4, 1))
  ),
  tibble::tibble(
    data_source = "fb-survey",
    signal = "smoothed_cli",
    geo_value = rep(c("ak", "ca", "fl", "tx", "wa"), each = 3),
    time_value = rep(as.Date("2021-01-01") + 0:2, times = 5),
    value = 15:1
  )
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

test_that("select_geo_top_n, filter is active, list provided", {
  selection_fn <- select_geo_top_n(n_locations = 3)
  expect_equal(selection_fn(df_state_wide),
               c("wa", "tx", "fl"))
})

test_that("select_geo_top_n, invalid n_locations", {
  expect_error(select_geo_top_n(n_locations = 0))
})
