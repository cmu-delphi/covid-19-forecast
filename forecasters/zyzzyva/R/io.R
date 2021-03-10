## functions for file input/output
## all functions should be named: `io.function_name()`

#' Load location information
#'
#' Location information (static in time) are provided through the "County
#' Health Rankings" datasets.  Therefore, legacy code may use this older
#' terminology.  From a modeling standpoint, any static covariates that
#' are location specific fall under the broader "location information"
#' umbrella, hence the renaming in this package.
#'
#' @param geo_type the geo type
#' @param location_covariates list of location-based covariates to be loaded from file 
#' @return tibble containing the location information
#'
#' @importFrom stringr str_to_upper
io.load_location_info <- function(geo_type,
                                  location_covariates) {
  raw_location_df <- readRDS(system.file(
                             "extdata",
                             paste0(geo_type, "-health-rankings.RDS"),
                             package="zyzzyva"))
  if ("location" %in% names(raw_location_df)) {
    if (any(grepl("HRR", pull(raw_location_df, location)))) {
      location_df <- raw_location_df %>%
        mutate(geo_value = gsub(".*_", "", location))
    }
  } else if ("fips" %in% names(raw_location_df)) {
    location_df <- raw_location_df %>%
      rename(geo_value = fips,
             population = chr_population)
  } else if ("state" %in% names(raw_location_df)) {
    location_df <- raw_location_df %>%
      mutate(geo_value = stringr::str_to_lower(state)) %>%
      rename(population = chr_population)
  }
  covariate_names <- sapply(location_covariates, function(x) x$name)
  location_df[c("geo_value", covariate_names)]
}
