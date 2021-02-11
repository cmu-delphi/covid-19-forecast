## #' Apply corrections, if available, to upstream data frame
## #'
## #' Corrections data are collection of replacement records for the
## #' original data. This means that the variables appear exactly in the
## #' same order as in the original record and only the `value` of a
## #' variable is potentially different. The replacement process returns
## #' a new tibble by removing the matching original data, matched by the
## #' variables `location`, `reference_date` and `variable_name` and
## #' appending the entire corrections data at the end. Ideally, this
## #' function should only make corrections that a properly versioned
## #' data frame cannot account for, i.e. persistent bad data rows that
## #' are likely to mess up forecasting algorithms (this has the salutory
## #' effect of keeping the number of corrections small). Note that
## #' `issue_date` is not accounted for; this function will have to
## #' modified to account for non-`NA` `issue_date`.
## #'
## #' @param df the upstream data frame corresponding to the geo type
## #' @param geo_type the geo_type corresponding to the upstream data
## #'     frame
## #' @param forecast_date the forecast date as a date object to account
## #'     for the response variable name change that happened on
## #'     `2020-07-13`
## #' @param log_info a boolean flag indicating whether to log
## #'     information on changes, default `TRUE`
## #' @return a df with corrections applied if the corrections are
## #'     available, or same dataframe
## #' @importFrom fs file_exists
## #' @importFrom dplyr anti_join select
## #' @importFrom logger log_info
## #' @importFrom magrittr %>%
## #' @export apply_corrections
## #'
## #' @examples
## #'
## #' \dontrun{
## #'   e  <- new.env()
## #'   load("upstream_df_state_2020-08-30.Rdata", envir = e)
## #'   new_df <- apply_corrections(df = e$df, geo_type = "state", forecast_date = lubridate::ymd("2020-08-09"))
## #'   nrow(e$df) == nrow(new_df)  # Same number of rows?
## #' }
## apply_corrections  <- function(df, geo_type = c("county", "state"), forecast_date,
##                                log_info = TRUE) {
##     geo_type  <- match.arg(geo_type)
##     if (geo_type == "state") {
##         corrections_file  <- system.file("extdata", "state_corrections.RDS", package = "evalforecast")
##     } else {
##         corrections_file  <- system.file("extdata", "county_corrections.RDS", package = "evalforecast")
##     }
##     if (fs::file_exists(corrections_file)) {
##         if (log_info) logger::log_info(sprintf("Reading corrections file for %s\n", geo_type))
##         corrections <- readRDS(corrections_file)
##         if (log_info) logger::log_info(sprintf("Applying %d row replacements\n", nrow(corrections)))
##         dplyr::anti_join(x = df, y = corrections, by = c("location", "reference_date", "variable_name")) %>%
##             dplyr::bind_rows(corrections)
##     } else {
##         if (log_info) logger::log_info(sprintf("No corrections available for %s\n", geo_type))
##         df
##     }
## }


#' Retrieve a tibble of data corrections from the corrections database
#'
#' Reads all records in the corrections database and returns a tibble
#' with columns noted above
#'
#' @param db_path the path for the SQLite database, for example
#'     `~/Github/covidcast-forecast/data_corrections/data_corrections.sqlite`
#' @param geo_type the `geo_type`, one of `"state"` or `"county"`
#' @return a tibble of corrections data
#' @importFrom dplyr tbl collect mutate
#' @importFrom magrittr %>%
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom lubridate ymd
#' @importFrom RSQLite SQLite
#' @examples
#' \dontrun{
#'   get_data_corrections(db_path = "~/Github/covidcast-forecast/data_corrections/data_corrections.sqlite", geo_type = "state")  ## 21-row tibble
#'   get_data_corrections(db_path = "~/Github/covidcast-forecast/data_corrections/data_corrections.sqlite", geo_type = "county")  ## 0-row tibble
#' }
#' @export get_data_corrections
get_data_corrections  <- function(db_path, geo_type) {
    ## The check below is preferable to match.arg because both state and county have same
    ## schema and we don't want people to clobber state with county data or vice-versa easily!
    if (!(geo_type %in% c("state", "county"))) {
        stop("get_data_corrections: geo_type should be one of 'state' or 'county'!")
    }
    con <- DBI::dbConnect(drv = RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(conn = con))
    dplyr::tbl(src = con, geo_type) %>%
        dplyr::collect() %>%
            dplyr::mutate(reference_date = lubridate::ymd(reference_date),
                          issue_date = lubridate::ymd(issue_date),
                          correction_date = lubridate::ymd(correction_date))
}


#' Replace corrections data for a geo type with new data
#'
#' Overwrites the corrections records for a geo type with new data after checking that schemas match.
#'
#' @param db_path the path for the SQLite database, for example
#'     `~/Github/covidcast-forecast/corrections/corrections.sqlite`
#' @param geo_type the `geo_type`, one of `"state"` or `"county"`
#' @param new_df the new data to replace the old
#' @return `TRUE` invisibly
#' @importFrom dplyr tbl collect mutate
#' @importFrom magrittr %>%
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom lubridate ymd
#' @importFrom RSQLite SQLite
#'
#' @examples
#' \dontrun{
#'   ## Make a test copy, so as not to clobber original
#'   file.copy(from = "~/Github/covidcast-forecast/data_corrections/data_corrections.sqlite", to = "./test.sqlite")
#'   d <- get_data_corrections(db_path = "./test.sqlite", geo_type = "state")  ## 21-row tibble
#'   ## Delete all rows but the first five
#'   d <- d[1:5, ]
#'   update_corrections("./test.sqlite", "state", d)
#'   get_data_corrections(db_path = "./test.sqlite", geo_type = "state")  ## 5-row tibble
#'   ## Modify d by adding a column
#'   d$foo <- 1
#'   update_corrections("./test.sqlite", "state", d) ## should fail
#' }
#' @export update_corrections
update_corrections  <- function(db_path, geo_type, new_df) {
    ## The check below is preferable to match.arg because both state and county have same
    ## schema and we don't want people to clobber state with county data or vice-versa easily!
    if (!(geo_type %in% c("state", "county"))) {
        stop("get_data_corrections: geo_type should be one of 'state' or 'county'!")
    }
    con <- DBI::dbConnect(drv = RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(conn = con))
    ## Ensure the schema is correct
    dplyr::tbl(src = con, geo_type) %>%
        dplyr::collect() %>%
            dplyr::mutate(reference_date = lubridate::ymd(reference_date),
                          issue_date = lubridate::ymd(issue_date),
                          correction_date = lubridate::ymd(correction_date)) ->
            current_data
    ## ## Quick and dirty way to check schema match
    ## current_schema  <- current_data[FALSE, ] ## get a zero row tibble of existing data
    ## new_schema  <- new_df[FALSE, ] ## zero row tibble of new data
    ## if (!identical(current_schema, new_schema)) {
    ##     stop("update_corrections: new_df does not have the same schema as old one!")
    ## }
    if (!identical_schema(current_data, new_df)) {
        stop("update_corrections: new_df does not have the same schema as old one!")
    }

    new_df %>%
        dplyr::mutate(reference_date = as.character(reference_date),
                      issue_date = as.character(issue_date),
                      correction_date = as.character(correction_date)
               ) %>%
        DBI::dbWriteTable(conn = con, name = geo_type, overwrite = TRUE)
    invisible(TRUE)
}


#' Apply corrections, if available, to upstream data frame
#'
#' Corrections data are replacement records for the original data
#' using the schema described above. This function returns a new
#' tibble by removing the matching original data, the matching being
#' done using the variables `location`, `reference_date` and
#' `variable_name`, and appending the entire corrections data at the
#' end. Ideally, this function should only make corrections that a
#' properly versioned data frame cannot account for, i.e. persistent
#' bad data rows that are likely to mess up forecasting algorithms
#' (this has the salutory effect of keeping the number of corrections
#' small).
#'
#' @param df the upstream data frame corresponding to the geo type
#' @param geo_type the geo_type corresponding to the upstream data
#'     frame
#' @param corrections_db_path the path for the SQLite database, for example
#'     `~/Github/covidcast-forecast/corrections/corrections.sqlite`
#' @return a df with corrections applied if the corrections are
#'     available, or same dataframe
#' @importFrom fs file_exists
#' @importFrom dplyr anti_join select rename mutate
#' @importFrom magrittr %>%
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom lubridate ymd
#' @importFrom RSQLite SQLite
#' @export apply_corrections
#'
#' @examples
#'
#' \dontrun{
#'   e  <- new.env()
#'   load("upstream_df_state_2020-09-06_final.Rdata", envir = e)
#'   new_df <- apply_corrections(df = e$df, geo_type = "state", "~/Github/covidcast-forecast/data_corrections/data_corrections.sqlite")
#'   nrow(e$df) == nrow(new_df)  # Same number of rows?
#' }
apply_corrections  <- function(df, geo_type, corrections_db_path) {
    ## The check below is preferable to match.arg because both state and county have same
    ## schema and we don't want people to clobber state with county data or vice-versa easily!
    if (!(geo_type %in% c("state", "county"))) {
        stop("get_data_corrections: geo_type should be one of 'state' or 'county'!")
    }

    con <- DBI::dbConnect(drv = RSQLite::SQLite(), corrections_db_path)
    on.exit(DBI::dbDisconnect(conn = con))


    dplyr::tbl(src = con, geo_type) %>%
        dplyr::collect() %>%
            dplyr::mutate(reference_date = lubridate::ymd(reference_date),
                          issue_date = lubridate::ymd(issue_date),
                          correction_date = lubridate::ymd(correction_date)) %>%
            ## Remove unneeded columns for merge
            dplyr::select(-value, -correction_date, -description) %>%
            dplyr::rename(value = new_value) %>% ## New value is now effective value
            dplyr::inner_join(x = df, by = c("location", "reference_date", "variable_name")) %>%
            ## New value in y overrides old value in x but old location_name and issue_date should prevail,
            ## so select columns in correct order and drop unneeded ones
            dplyr::select(location, location_name = location_name.x, reference_date, issue_date = issue_date.x,
                          variable_name, value = value.y, -value.x, -location_name.y, -issue_date.y) ->
            corrections

    if (nrow(corrections) == 0) {
        warning(sprintf("No corrections available for %s", geo_type))
        return(df)
    } else {
        message(sprintf("%d correction records available for %s", nrow(corrections), geo_type))
    }

    ## ## Ensure the schema is correct. Quick and dirty way to check schema match:
    ## ## get a zero row tibble of existing data
    ## current_schema  <- corrections[FALSE, ]
    ## df_schema  <- df[FALSE, ] ## zero row tibble of new data
    ## if (!identical(current_schema, df_schema)) {
    ##     stop("apply_corrections: df does not have the same schema as corrections!")
    ## }

    ## Ensure the schema is correct. Quick and dirty way to check schema match:
    ## get a zero row tibble of existing data
    if (!identical_schema(corrections, df)) {
        stop("apply_corrections: df does not have the same schema as corrections!")
    }

    dplyr::anti_join(x = df, y = corrections,
                     by = c("location", "reference_date", "variable_name")) %>%
        dplyr::bind_rows(corrections)

}

#' A simple check to ensure two data frames have same column names and classes
#' @param x the first df
#' @param y the second df
#' @return a boolean `TRUE` or `FALSE`
#'
identical_schema  <- function(x, y) {
    ## Quick and dirty way to check schema match
    identical(names(x), names(y)) &&
        identical(lapply(x, class), lapply(y, class))
}
