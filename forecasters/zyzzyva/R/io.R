## functions for file input/output
## all functions should be named: `io.function_name()`

STATE_TO_FIPS = list(
    'WA'='53', 'DE'='10', 'DC'='11', 'WI'='55', 'WV'='54', 'HI'='15',
    'FL'='12', 'WY'='56', 'PR'='72', 'NJ'='34', 'NM'='35', 'TX'='48',
    'LA'='22', 'NC'='37', 'ND'='38', 'NE'='31', 'TN'='47', 'NY'='36',
    'PA'='42', 'AK'='02', 'NV'='32', 'NH'='33', 'VA'='51', 'CO'='08',
    'CA'='06', 'AL'='01', 'AR'='05', 'VT'='50', 'IL'='17', 'GA'='13',
    'IN'='18', 'IA'='19', 'MA'='25', 'AZ'='04', 'ID'='16', 'CT'='09',
    'ME'='23', 'MD'='24', 'OK'='40', 'OH'='39', 'UT'='49', 'MO'='29',
    'MN'='27', 'MI'='26', 'RI'='44', 'KS'='20', 'MT'='30', 'MS'='28',
    'SC'='45', 'KY'='21', 'OR'='41', 'SD'='46',
    'AS'='60', 'GU'='66', 'MP'='69', 'VI'='78', 'UM'='74'
)

#' Load location information
#'
#' Location information (static in time) are provided through the "County
#' Health Rankings" datasets.  Therefore, legacy code may use this older
#' terminology.  From a modeling standpoint, any static covariates that
#' are location specific fall under the broader "location information"
#' umbrella, hence the renaming in this package.
#'
#' @param geo_type the geo type
#' @return tibble containing the location information
#'
#' @importFrom stringr str_to_upper
io.load_location_info <- function(geo_type) {
  raw_location_df <- readRDS(system.file(
                             "extdata",
                             paste0(geo_type, "-health-rankings.RDS"),
                             package="zyzzyva"))
  if ("location" %in% names(raw_location_df)) {
    if (any(grepl("HRR", pull(raw_location_df, location)))) {
      location_df <- raw_location_df %>%
        mutate(location = gsub(".*_", "", location))
    }
  } else if ("fips" %in% names(raw_location_df)) {
    location_df <- raw_location_df %>% rename(
        location = fips,
        population = chr_population,
      ) %>% select (
        -state,
        -county,
      )
  } else if ("state" %in% names(raw_location_df)) {
    location_df <- raw_location_df %>%  mutate(
        location_name = stringr::str_to_upper(state),
        location = sapply(location_name, function(x) STATE_TO_FIPS[[x]])
      ) %>% rename (
        population = chr_population,
      ) %>% select (
        -state,
        -location_name,
      )
  }
  location_df
}
