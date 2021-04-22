library(covidcast)
library(dplyr)

state_geo_values <- c("ak", "al", "az")
state_population <- c(731545, 4903185, 7278717)
county_geo_values <- c("06037", "17031", "49035")
county_population <- c(10039107, 5150233, 1160437)

state_df_list <- covidcast::covidcast_signals(
    data_source = c("jhu-csse","jhu-csse"),
    signal = c("deaths_incidence_num", "confirmed_incidence_num"),
    start_day = "2020-11-01",
    end_day = "2020-11-01",
    geo_type = "state",
    geo_values = state_geo_values,
    as_of = "2020-11-08"
)

county_df_list <- covidcast::covidcast_signals(
    data_source = c("jhu-csse","jhu-csse"),
    signal = c("deaths_incidence_num", "confirmed_incidence_num"),
    start_day = "2020-11-01",
    end_day = "2020-11-01",
    geo_type = "county",
    geo_values = county_geo_values,
    as_of = "2020-11-08"
)

test_that("normalize_by_population for state, no normalization", {
    expect_equal(normalize_by_population(state_df_list, FALSE),
                 state_df_list)
})

test_that("normalize_by_population for state, normalize all signals", {
    norm_state_df_list <- normalize_by_population(state_df_list, TRUE)
    expect_equal(norm_state_df_list[[1]]$value, 
                 state_df_list[[1]]$value / state_population * 1e5)
    expect_equal(norm_state_df_list[[2]]$value, 
                 state_df_list[[2]]$value / state_population * 1e5)
})

test_that("normalize_by_population for state, normalize only 2nd signal", {
    norm_state_df_list <- normalize_by_population(state_df_list, 
                                                  list(FALSE, TRUE))
    expect_equal(norm_state_df_list[[1]]$value, 
                 state_df_list[[1]]$value)
    expect_equal(norm_state_df_list[[2]]$value, 
                 state_df_list[[2]]$value / state_population * 1e5)
})

test_that("normalize_by_population for state, different base", {
    norm_state_df_list <- normalize_by_population(state_df_list, TRUE, 
                                                  base = 100)
    expect_equal(norm_state_df_list[[1]]$value, 
                 state_df_list[[1]]$value / state_population * 100)
    expect_equal(norm_state_df_list[[2]]$value, 
                 state_df_list[[2]]$value / state_population * 100)
})

test_that("normalize_by_population, incorrect length for norm_by_popn", {
    expect_error(normalize_by_population(state_df_list, 
                                         list(FALSE, TRUE, TRUE)))
})

test_that("normalize_by_population for county, no normalization", {
    expect_equal(normalize_by_population(county_df_list, FALSE),
                 county_df_list)
})

test_that("normalize_by_population for county, normalize all signals", {
    norm_county_df_list <- normalize_by_population(county_df_list, TRUE)
    expect_equal(norm_county_df_list[[1]]$value, 
                 county_df_list[[1]]$value / county_population * 1e5)
    expect_equal(norm_county_df_list[[2]]$value, 
                 county_df_list[[2]]$value / county_population * 1e5)
})

test_that("normalize_by_population for county, normalize only 2nd signal", {
    norm_county_df_list <- normalize_by_population(county_df_list, 
                                                  list(FALSE, TRUE))
    expect_equal(norm_county_df_list[[1]]$value, 
                 county_df_list[[1]]$value)
    expect_equal(norm_county_df_list[[2]]$value, 
                 county_df_list[[2]]$value / county_population * 1e5)
})

test_that("invnorm_by_population for state", {
    df <- data.frame(geo_value = rep(state_geo_values, 2),
                     value = 0:5)
    expect_equal(invnorm_by_population(df, "state"),
                 data.frame(geo_value = rep(state_geo_values, 2),
                            value = 0:5 / 1e5 * rep(state_population, 2)))
})

test_that("invnorm_by_population for state, different base", {
    df <- data.frame(geo_value = rep(state_geo_values, 2),
                     value = 0:5)
    expect_equal(invnorm_by_population(df, "state", base = 10),
                 data.frame(geo_value = rep(state_geo_values, 2),
                            value = 0:5 / 10 * rep(state_population, 2)))
})

test_that("invnorm_by_population for county", {
    df <- data.frame(geo_value = rep(county_geo_values, 2),
                     value = 0:5)
    expect_equal(invnorm_by_population(df, "county"),
                 data.frame(geo_value = rep(county_geo_values, 2),
                            value = 0:5 / 1e5 * rep(county_population, 2)))
})