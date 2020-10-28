
#' Apply corrections to case/death data prior to forecasting
#'
#' @param df
#'
#' @return a data frame with additional columns
#' @export
#'
#' @importFrom dplyr filter left_join
zookeeper_corrections <- function(df){

  # first grab the response, evalcast expects this in the first row of signals
  # and hence in the first row of df
  resp = unlist(df[1,])
  to_correct = dplyr::filter(df, data_source == resp[1], signal == resp[2])

  corrected = switch(
    resp$geo_type,
    county = county_subsetter(to_correct, correction_pars) %>%
      county_corrector(correction_pars),
    state = state_corrector(to_correct, correction_pars),
    to_correct
  )

  to_save = dplyr::left_join(to_correct, corrected)
  saveRDS(to_save, file="corrections.RDS") # for later visualization

  corrected
}
