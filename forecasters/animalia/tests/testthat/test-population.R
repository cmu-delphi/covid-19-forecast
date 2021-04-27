library(covidcast)
library(dplyr)

state_geo_values <- c("ak", "al", "az")
state_population <- c(731545, 4903185, 7278717)
county_geo_values <- c("06037", "17031", "49035")
county_population <- c(10039107, 5150233, 1160437)

state_df_list <- list(
    data.frame(geo_value = rep(state_geo_values, 2),
               value = 0:5),
    data.frame(geo_value = rep(state_geo_values, 2),
               value = 10:15)
)

county_df_list <- list(
    data.frame(geo_value = rep(county_geo_values, 2),
               value = 0:5),
    data.frame(geo_value = rep(county_geo_values, 2),
               value = 10:15)
)

test_that("normalize_by_population, incorrect length for signals_to_normalize", {
    expect_error(normalize_by_population(state_df_list, "state",
                                         c(FALSE, TRUE, TRUE)))
})

test_that("normalize_by_population, incorrect length for geo_type", {
    expect_error(normalize_by_population(state_df_list, rep("state", 3),
                                         TRUE))
})

test_that("normalize_by_population for state, no normalization", {
    expect_equal(normalize_by_population(state_df_list, "state", FALSE),
                 state_df_list)
})

test_that("normalize_by_population for state, normalize all signals", {
    norm_state_df_list <- normalize_by_population(state_df_list, "state", TRUE)
    expect_equal(norm_state_df_list[[1]]$value, 
                 state_df_list[[1]]$value / state_population * 1e5)
    expect_equal(norm_state_df_list[[2]]$value, 
                 state_df_list[[2]]$value / state_population * 1e5)
    
    # unnormalization
    norm_state_df_list <- normalize_by_population(state_df_list, "state", TRUE,
                                                  invert = TRUE)
    expect_equal(norm_state_df_list[[1]]$value, 
                 state_df_list[[1]]$value / 1e5 * state_population)
    expect_equal(norm_state_df_list[[2]]$value, 
                 state_df_list[[2]]$value / 1e5 * state_population)
})

test_that("normalize_by_population for state, normalize only 2nd signal", {
    norm_state_df_list <- normalize_by_population(state_df_list, "state",
                                                  c(FALSE, TRUE))
    expect_equal(norm_state_df_list[[1]]$value, 
                 state_df_list[[1]]$value)
    expect_equal(norm_state_df_list[[2]]$value, 
                 state_df_list[[2]]$value / state_population * 1e5)
    
    # unnormalization
    norm_state_df_list <- normalize_by_population(state_df_list, "state",
                                                  c(FALSE, TRUE),
                                                  invert = TRUE)
    expect_equal(norm_state_df_list[[1]]$value, 
                 state_df_list[[1]]$value)
    expect_equal(norm_state_df_list[[2]]$value, 
                 state_df_list[[2]]$value / 1e5 * state_population)
})

test_that("normalize_by_population for state, different base", {
    norm_state_df_list <- normalize_by_population(state_df_list, "state", TRUE, 
                                                  base = 100)
    expect_equal(norm_state_df_list[[1]]$value, 
                 state_df_list[[1]]$value / state_population * 100)
    expect_equal(norm_state_df_list[[2]]$value, 
                 state_df_list[[2]]$value / state_population * 100)
    
    # unnormalization
    norm_state_df_list <- normalize_by_population(state_df_list, "state", TRUE, 
                                                  base = 100, invert = TRUE)
    expect_equal(norm_state_df_list[[1]]$value, 
                 state_df_list[[1]]$value / 100 * state_population)
    expect_equal(norm_state_df_list[[2]]$value, 
                 state_df_list[[2]]$value / 100 * state_population)
})

test_that("normalize_by_population for state, only one signal provided", {
    norm_state_df <- normalize_by_population(state_df_list[[1]], "state", TRUE)
    expect_equal(norm_state_df$value, 
                 state_df_list[[1]]$value / state_population * 1e5)
    
    # unnormalization
    norm_state_df <- normalize_by_population(state_df_list[[1]], "state", TRUE,
                                             invert = TRUE)
    expect_equal(norm_state_df$value, 
                 state_df_list[[1]]$value / 1e5 * state_population)
})

test_that("normalize_by_population for county, no normalization", {
    expect_equal(normalize_by_population(county_df_list, "county", FALSE),
                 county_df_list)
})

test_that("normalize_by_population for county, normalize all signals", {
    norm_county_df_list <- normalize_by_population(county_df_list, "county", TRUE)
    expect_equal(norm_county_df_list[[1]]$value, 
                 county_df_list[[1]]$value / county_population * 1e5)
    expect_equal(norm_county_df_list[[2]]$value, 
                 county_df_list[[2]]$value / county_population * 1e5)
})

test_that("normalize_by_population for county, normalize only 2nd signal", {
    norm_county_df_list <- normalize_by_population(county_df_list, "county", 
                                                  c(FALSE, TRUE))
    expect_equal(norm_county_df_list[[1]]$value, 
                 county_df_list[[1]]$value)
    expect_equal(norm_county_df_list[[2]]$value, 
                 county_df_list[[2]]$value / county_population * 1e5)
})