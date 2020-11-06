## ----get_data_corrections-----------------------------------------------------
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


## ----update_corrections-------------------------------------------------------
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
    ## Quick and dirty way to check schema match
    current_schema  <- current_data[FALSE, ] ## get a zero row tibble of existing data
    new_schema  <- new_df[FALSE, ] ## zero row tibble of new data
    if (!identical(current_schema, new_schema)) {
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


## ----apply_corrections--------------------------------------------------------
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

    ## Ensure the schema is correct
    dplyr::tbl(src = con, geo_type) %>%
        dplyr::collect() %>%
            dplyr::mutate(reference_date = lubridate::ymd(reference_date),
                          issue_date = lubridate::ymd(issue_date),
                          correction_date = lubridate::ymd(correction_date)) %>%
            dplyr::select(-value, -correction_date, -description) %>% ## Remove old value
            dplyr::rename(value = new_value) -> ## Rename new_value
            corrections
    if (nrow(corrections) == 0) {
        warning(sprintf("No corrections available for %s", geo_type))
        return(df)
    }
    
    ## Quick and dirty way to check schema match
    ## get a zero row tibble of existing data
    current_schema  <- corrections[FALSE, ]
    df_schema  <- df[FALSE, ] ## zero row tibble of new data
    if (!identical(current_schema, df_schema)) {
        stop("apply_corrections: df does not have the same schema as corrections!")
    }
    dplyr::anti_join(x = df, y = corrections,
                     by = c("location", "reference_date", "variable_name")) %>%
        dplyr::bind_rows(corrections)
}

