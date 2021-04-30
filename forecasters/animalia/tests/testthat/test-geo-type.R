test_that("get_geo_type, state", {
  df <- data.frame(geo_value = c("ak", "al", "az"),
                   value = 1:3)
  expect_equal(get_geo_type(df), "state")
})

test_that("get_geo_type, county", {
  df <- data.frame(geo_value = c("06037", "17031", "49035"),
                   value = 1:3)
  expect_equal(get_geo_type(df), "county")
})

test_that("get_geo_type, error", {
  df <- data.frame(geo_value = c("arkansas", "alabama", "arizona"),
                   value = 1:3)
  expect_error(get_geo_type(df))
})