test_that("format_predictions_for_reichlab_submission works", {
    pcard1 <- tibble(
      ahead = rep(c(1, 2, 3, 4), each = 5),
      geo_value = "pa",
      quantile = rep(c(0.1, 0.4, 0.5, 0.6, 0.9), 4),
      value = 1:20,
      forecaster = "a",
      forecast_date = as.Date("2020-01-02"),
      data_source = "source",
      signal = rep(c("confirmed_incidence_num", "confirmed_incidence_num",
                     "deaths_incidence_num", "confirmed_admissions_covid_1d"),
                   each = 5),
      target_end_date = rep(as.Date(c("2020-01-09", "2020-01-16", "2020-01-23", "2020-01-06")), each = 5),
      incidence_period = c(rep("epiweek", 15), rep("day", 5))
    )
    class(pcard1) <- c("predictions_cards", class(pcard1))

    out <- format_predictions_for_reichlab_submission(pcard1)
    expect_equal(names(out), c("location", "forecast_date", "quantile", "value", "target",
                               "target_end_date", "type"))
    expect_equal(out$location, rep("42", 20))
    expect_equal(out$forecast_date, rep(as.Date("2020-01-02"), 20))
    expect_equal(out$quantile, c(0.1, 0.5, 0.9, 0.1, 0.5, 0.9, 0.1, 0.4, 0.5, 0.6, 0.9, 0.1, 0.4, 0.5, 0.6, 0.9, NA, NA, NA, NA))
    expect_equal(out$value, c(1, 3, 5, 6, 8, 10, 11:20, 3, 8, 13, 18))
    expect_equal(out$target, c("1 wk ahead inc case",
                               "1 wk ahead inc case",
                               "1 wk ahead inc case",
                               "2 wk ahead inc case",
                               "2 wk ahead inc case",
                               "2 wk ahead inc case",
                               "3 wk ahead inc death",
                               "3 wk ahead inc death",
                               "3 wk ahead inc death",
                               "3 wk ahead inc death",
                               "3 wk ahead inc death",
                               "4 day ahead inc hosp",
                               "4 day ahead inc hosp",
                               "4 day ahead inc hosp",
                               "4 day ahead inc hosp",
                               "4 day ahead inc hosp",
                               "1 wk ahead inc case",
                               "2 wk ahead inc case",
                               "3 wk ahead inc death",
                               "4 day ahead inc hosp"
                               ))
    expect_equal(out$target_end_date, as.Date(c("2020-01-09", "2020-01-09", "2020-01-09",
                                                "2020-01-16", "2020-01-16", "2020-01-16",
                                                rep("2020-01-23", 5),
                                                rep("2020-01-06", 5),
                                                "2020-01-09", "2020-01-16", "2020-01-23",
                                                "2020-01-06")))
    expect_equal(out$type, c(rep("quantile", 16), rep("point", 4)))
})

test_that("format_predictions_for_reichlab_submission filters locations", {
    pcard <- tibble(
      ahead = rep(c(1, 2, 3), each = 5),
      geo_value = rep(c("al", "pa", "wy"), each = 5),
      quantile = rep(c(0.1, 0.4, 0.5, 0.6, 0.9), 3),
      value = seq(1, 15),
      forecaster = "a",
      forecast_date = as.Date("2020-01-02"),
      data_source = "source",
      signal = rep(c("confirmed_incidence_num", "confirmed_incidence_num", "deaths_incidence_num"),
                   each = 5),
      target_end_date = rep(as.Date(c("2020-01-09", "2020-01-16", "2020-01-23")), each = 5),
      incidence_period = "epiweek"
    )
    class(pcard) <- c("predictions_cards", class(pcard))
    out <- format_predictions_for_reichlab_submission(pcard, "pa")

    expect_equal(names(out), c("location", "forecast_date", "quantile", "value", "target",
                               "target_end_date", "type"))
    expect_equal(out$location, c("01", "01", "01", "56", "56", "56", "56", "56", "01", "56"))
    expect_equal(out$forecast_date, rep(as.Date("2020-01-02"), 10))
    expect_equal(out$quantile, c(0.1, 0.5, 0.9, 0.1, 0.4, 0.5, 0.6, 0.9, NA, NA))
    expect_equal(out$value, c(1, 3, 5, 11, 12, 13, 14, 15, 3, 13))
    expect_equal(out$target, c("1 wk ahead inc case",
                               "1 wk ahead inc case",
                               "1 wk ahead inc case",
                               "3 wk ahead inc death",
                               "3 wk ahead inc death",
                               "3 wk ahead inc death",
                               "3 wk ahead inc death",
                               "3 wk ahead inc death",
                               "1 wk ahead inc case",
                               "3 wk ahead inc death"
                               ))
    expect_equal(out$target_end_date, as.Date(c("2020-01-09", "2020-01-09", "2020-01-09",
                                                "2020-01-23", "2020-01-23", "2020-01-23",
                                                "2020-01-23", "2020-01-23", "2020-01-09",
                                                "2020-01-23")))
    expect_equal(out$type, c(rep("quantile", 8), rep("point", 2)))
})
