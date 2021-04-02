#' County population data
#'
#' Data set on county populations, from the 2019 US Census.
#'
#' @format A data frame with 3193 rows, one for each county (along with the 50
#'   states and DC). There are many columns. The most crucial are:  
#'
#' \describe{
#'   \item{geo_value}{Five-digit county FIPS codes. These are unique identifiers
#'   used, for example, as the `geo_values` argument to `covidcast_signal()` to
#'   request data from a specific county.}
#'   \item{population}{Estimate of the county's resident population as of
#'   July 1, 2019.}
#' }
#'
#' @references Census Bureau documentation of all columns and their meaning:
#'   \url{https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.pdf}
#'
#' @source United States Census Bureau, at
#'   \url{https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv}
#'
#' @seealso [covidcast::county_census]
"county_population"

#' State population data
#'
#' Data set on state populations, from the 2019 US Census.
#'
#' @format Data frame with 53 rows (including one for the United States as a
#'   whole, plus the District of Columbia and the Puerto Rico Commonwealth).
#'
#' \describe{
#'   \item{geo_value}{2 letter abbreviation of the state / territory}
#'   \item{population}{Estimate of the location's resident population in 2019.}
#' }
#'
#' @source United States Census Bureau, at
#'   \url{https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/state/detail/SCPRC-EST2019-18+POP-RES.csv}
#'
#' @seealso [covidcast::state_census]
"state_population"