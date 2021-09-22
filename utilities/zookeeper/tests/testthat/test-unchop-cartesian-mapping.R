testthat::test_that('`unchop_cartesian_mapping` examples yield expected output', {
  tbl1 <- tibble::tribble(
    ~location, ~time, ~unique_time_description,
    c("AK","AL"), c(as.Date("2021-01-01"), as.Date("2021-01-02"), as.Date("2021-01-03")), c("New Year's Day","Other","Other"),
    "AK", as.Date("2021-03-29"), "Seward's Day"
  )
  testthat::expect_equal(
              unchop_cartesian_mapping(
                tbl1,
                location, time ~ unique_time_description
              ),
              tibble::tibble(
                        location = c(rep(c("AK","AL"), each=3L), "AK"),
                        time = c(rep(c(as.Date("2021-01-01"), as.Date("2021-01-02"), as.Date("2021-01-03")), 2L), as.Date("2021-03-29")),
                        unique_time_description = c(rep(c("New Year's Day","Other","Other"), 2L), "Seward's Day"),
                      )
            )

  ## In the above example, there must not be duplicate (location, time) values
  ## in the result. In the below examples, duplicate (location, time) values
  ## are allowed, but entirely duplicated rows are not.

  tbl2 <- tibble::tribble(
    ~location, ~time, ~time_tag,
    c("AK","AL"), as.Date("2021-03-29"), "Not New Year's",
    "AK", as.Date("2021-03-29"), "Seward's Day"
  )
  testthat::expect_equal(
              unchop_cartesian_mapping(
                tbl2,
                location, starts_with("time")
              ),
              tibble::tibble(
                        location = c(c("AK","AL"), "AK"),
                        time = c(rep(as.Date("2021-03-29"),2L), as.Date("2021-03-29")),
                        time_tag = c(rep("Not New Year's",2L), "Seward's Day")
                      )
            )

  tbl3 <- tibble::tribble(
    ~time, ~location, ~tag,
    as.Date("2021-01-01"), c("AK","AL"), "New Year's Day",
    c(as.Date("2021-01-02"),as.Date("2021-01-03")), c("AK","AL"), "Other",
    as.Date("2021-03-29"), c("AK","AL"), c("Seward's Day","Other")
  )
  testthat::expect_equal(
              unchop_cartesian_mapping(
                tbl3,
                time, location ~ tag
              ),
              tibble::tibble(
                        time = c(rep(as.Date("2021-01-01"), each=2L),
                                 rep(c(as.Date("2021-01-02"),as.Date("2021-01-03")), each=2L),
                                 rep(as.Date("2021-03-29"), each=2L)),
                        location = c(rep(c("AK","AL"), 1L),
                                     rep(c("AK","AL"), 2L),
                                     rep(c("AK","AL"), 1L)),
                        tag = c(rep(rep("New Year's Day", 2L), 1L),
                                rep(rep("Other", 2L), 2L),
                                rep(c("Seward's Day", "Other"), 1L))
                      )
            )
})

testthat::test_that('`unchop_cartesian_mapping` does not accept one-sided formulas', {
  testthat::expect_error(unchop_cartesian_mapping(tibble::tibble(a=1,b=2), ~c(a,b)),
                         'formula selectors must have both')
})

testthat::test_that('`unchop_cartesian_mapping` catches column partition issues', {
  test_tbl <- tibble::tibble(a=1,b=2)
  testthat::expect_error(unchop_cartesian_mapping(test_tbl),
                         'matched exactly once.*zero times: a, b;.*multiple times: $')
  testthat::expect_error(unchop_cartesian_mapping(test_tbl, a),
                         'matched exactly once.*zero times: b;.*multiple times: $')
  testthat::expect_error(unchop_cartesian_mapping(test_tbl, a, a, b),
                         'matched exactly once.*zero times: ;.*multiple times: a$')
  testthat::expect_error(unchop_cartesian_mapping(test_tbl, a ~ b, a),
                         'matched exactly once.*zero times: ;.*multiple times: a$')
  testthat::expect_error(unchop_cartesian_mapping(test_tbl, where(is.numeric), c(a,b)),
                         'matched exactly once.*zero times: ;.*multiple times: a, b$')
  testthat::expect_error(unchop_cartesian_mapping(test_tbl, a, a),
                         'matched exactly once.*zero times: b;.*multiple times: a$')
})

testthat::test_that('`unchop_cartesian_mapping` catches groups with no keys', {
  test_tbl <- tibble::tibble(a=1,b="B")
  testthat::expect_error(unchop_cartesian_mapping(test_tbl, c() ~ c(a,b)),
                         'Each column group must contain at least one key column.')
  testthat::expect_error(unchop_cartesian_mapping(test_tbl, a, starts_with("c") ~ b),
                         'Each column group must contain at least one key column.')
})

testthat::test_that('`unchop_cartesian_mapping` catches duplicate composite keys', {
  ## Within one original row
  testthat::expect_error(unchop_cartesian_mapping(
              tibble::tribble(~a, ~k, ~v,
                              c("A", "B"), 2, 2:3,
                              "C", 2, 5
                              ),
              a, k ~ v
              ), 'Duplicate composite key values found while unchopping; e.g., a=A, k=2')
  ## Between original rows
  testthat::expect_error(unchop_cartesian_mapping(
              tibble::tribble(~a, ~k, ~v,
                              c("A", "B"), 2:3, 2,
                              "A", 2, 5
                              ),
              a, k ~ v
              ), 'Duplicate composite key values found while unchopping; e.g., a=A, k=2')
})
