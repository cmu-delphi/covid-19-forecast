test_that("mob_trans_shift works", {
  x = c(rep(1, 7), 2)
  expect_equal(mob_trans_shift(x, 7, "mean"), 1)
})
