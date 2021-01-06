test_that("resp_trans works", {
  x = c(2, runif(7))
  expect_equal(resp_trans(x, 7), mob_trans(x, 7, "mean") * 7)
})
