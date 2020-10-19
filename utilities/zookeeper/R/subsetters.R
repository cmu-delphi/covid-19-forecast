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
  tokeep = arrange(todump, desc(case_sum)) %>% top_n(total_counties, wt=case_sum)
  dplyr::filter(x, location %in% tokeep$geo_value)
}
