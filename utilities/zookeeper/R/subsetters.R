#' Only applies corrections to some county data
#'
#' @param x a data frame as returned by `evalcast:::download_signal()`
#' @param params a list of parameters, typically `correction_pars`
#'
#' @return a subset of x
#' @export
#' @importFrom dplyr group_by summarise filter arrange desc filter
county_subsetter <- function(x, params){
  x = dplyr::group_by(x, location)
  tokeep = dplyr::summarise(
    x,
    ava_value_count = sum((!is.na(value) | (value > 0))),
    case_sum = sum(value, na.rm = T),
    max_case = max(value))
  tokeep = with(params,
                dplyr::filter(
                  todump, max_case >= max_county_cases,
                  ava_value_count >= min_county_reports,
                  as.numeric(location) %% 1000 > 0))
  tokeep = dplyr::arrange(todump, dplyr::desc(case_sum)) %>%
    dplyr::top_n(total_counties, wt=case_sum)
  dplyr::filter(x, location %in% tokeep$geo_value)
}
