#' Helper function to get geo_type for COVIDcast signals
#' 
#' Helper function to get `geo_type` ("state" or "county") for COVIDcast 
#' signals. A bit of a hack: it looks at the number of characters in the 
#' first element in the `geo_value` column and makes a determination.
#' 
#' @param df A data.frame corresponding to a single COVIDcast signal.
#' 
#' @return The `geo_type` of the signal ("state" or "county").
#' 
get_geo_type <- function(df) {
  string_length <- nchar(df$geo_value[1])
  if (string_length == 2) {
    return("state")
  } else if (string_length == 5) {
    return("county")
  } else {
    stop("geo_value string has length other than 2 or 5.")
  }
}