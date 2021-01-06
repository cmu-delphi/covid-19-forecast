test_that("mob_trans works", {
  x = c(2, rep(1, 7))
  expect_equal(mob_trans(x, 7, "mean"), 1)
})
