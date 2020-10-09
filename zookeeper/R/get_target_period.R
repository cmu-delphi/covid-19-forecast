## Used in aardvark, stolen from evalforecast
##
## This function IS in evalcast once available, to be removed from here


#' @export
#' @importFrom MMWRweek MMWRweek MMWRweek2Date
#' @importFrom lubridate wday
#' @importFrom tibble tibble
get_target_period <- function(forecast_date, incidence_period, ahead) {
  # This function gives the start and end dates of the target period,
  # based on the system described in the CDC competition rules here:
  # https://github.com/reichlab/covid19-forecast-hub/blob/master/data-processed/README.md
  #
  # Inputs:
  #  forecast_date: can be a vector of dates
  #  incidence_period: one of "epiweek" or "day"
  #  ahead: how many epiweeks/days ahead are you forecasting?
  if (incidence_period == "day") {
    return(tibble::tibble(
      start = forecast_date + ahead,
      end = forecast_date + ahead))
  }
  if (incidence_period != "epiweek") stop("Unsupported incidence_period")
  # incidence_period: epiweek
  ew_frcst_date <- MMWRweek::MMWRweek(forecast_date) # get epiweek of forecast_dates
  sunday_of_ew_frcst_date <- MMWRweek::MMWRweek2Date(
    MMWRyear = ew_frcst_date$MMWRyear,
    MMWRweek = ew_frcst_date$MMWRweek,
    MMWRday = 1) # 1 is Sunday
  # From https://github.com/reichlab/covid19-forecast-hub/blob/master/data-processed/README.md:
  # "For week-ahead forecasts with forecast_date of Sunday or Monday of EW12, a
  # 1 week ahead forecast corresponds to EW12 and should have target_end_date of
  # the Saturday of EW12. For week-ahead forecasts with forecast_date of Tuesday
  # through Saturday of EW12, a 1 week ahead forecast corresponds to EW13 and
  # should have target_end_date of the Saturday of EW13."
  week_ahead <- ifelse(
    lubridate::wday(forecast_date) <= 2, # forecasting on a Sun/Monday
    ahead - 1,
    ahead)
  tibble::tibble(
    start = sunday_of_ew_frcst_date + week_ahead * 7,
    end = sunday_of_ew_frcst_date + (week_ahead + 1) * 7 - 1)
}
