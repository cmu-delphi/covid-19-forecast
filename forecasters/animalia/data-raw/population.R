## code to prepare population datasets
library(tidyverse)
county_population <- covidcast::county_census %>%
  as_tibble() %>%
  select(FIPS, POPESTIMATE2019) %>%
  rename(geo_value = "FIPS", population = "POPESTIMATE2019")
usethis::use_data(county_population, overwrite = TRUE)

state_population <- covidcast::state_census %>%
  as_tibble() %>%
  select(ABBR, POPESTIMATE2019) %>%
  rename(geo_value = "ABBR", population = "POPESTIMATE2019") %>%
  mutate(geo_value = tolower(geo_value))
usethis::use_data(state_population, overwrite = TRUE)
