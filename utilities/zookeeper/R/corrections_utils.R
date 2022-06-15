#' @source wrappers.R
NULL

na_replace <- function(a, b) {
  stopifnot(length(a) == length(b) || length(b) == 1)
  a[is.na(a)] <- b[is.na(a)]
  a
}

exp_w <- function(x, std_decay = 30, b0 = 8, a = exp(1) / 2){
  stopifnot(length(x) <= std_decay)
  w <- (1:std_decay) / std_decay
  w <- utils::tail(w, length(x))
  1 / (1 + exp(-w * b0 + a))
}

missing_future <- function(selector, time_value, excess, preds) {
  if (!any(excess > 0)) return(0L)
  local_tail <- (selector & time_value > time_value[max(which(excess > 0))])
  if (!any(local_tail)) return(0L)
  tot <- round(sum(preds[local_tail], na.rm = TRUE))
  if (tot <= 0) return(0L)
  rmultinom_wrapper(1, tot, as.numeric(local_tail))
}

replace_manual <- function(selector, original, replacement) {
  if (sum(selector) != length(replacement)) {
    warning("replacement length does not equal the number of values being replaced")
    return(0L)
  }
  original[selector] <- replacement
  original
}

corrections_multinom_roll <- function(
  x, excess, flag, time_value, max_lag = Inf, expectations = NULL,
  inc_out_time = TRUE, reweight = function(x) rep(1, length(x))) {
  if (length(max_lag) == 1L) {
    max_lag <- rep(max_lag, length(x))
  }
  stopifnot(length(x) == length(excess), length(excess) == length(flag), length(flag) == length(time_value), length(time_value) == length(max_lag))
  stopifnot(is.logical(flag))
  max_lag_at_flags <- max_lag[which(flag)] # (treats NA flags as FALSE)
  stopifnot(all(max_lag_at_flags == floor(max_lag_at_flags)), all(max_lag_at_flags >= 1))
  if (!is.null(expectations) && length(expectations) == 1) expectations <- NULL
  if (length(expectations) > 1) stopifnot(length(expectations) == length(x))

  locs <- which(flag)
  if (length(locs) == 0) return(x)
  if (is.null(expectations)) expectations <- rep(1, length(x))

  excess[is.na(excess)] <- 0
  bad_excess <- abs(excess %% 1) > 1e-10
  if (any(bad_excess)) {
    excess[bad_excess] <- floor(excess[bad_excess]) +
      rbinom_wrapper(sum(bad_excess), 1, .5)
  }

  for (ii in locs) {
    if (ii <= max_lag[[ii]]) {
      ii_lag <- 1:(ii - 1 + inc_out_time)
    } else {
      ii_lag <- seq(ii - max_lag[[ii]] + 1, ii - 1 + inc_out_time)
    }

    bin_w <- pmax(expectations[ii_lag] /
                    sum(expectations[ii_lag], na.rm = TRUE), 0)
    bin_w[is.na(bin_w)] <- 0

    if (all(bin_w == 0)) bin_w <- rep(1 / length(ii_lag), times = length(ii_lag))

    #reweight bin_w
    zz <- reweight(bin_w)
    bin_w <- zz*bin_w

    x[ii] <- x[ii] - excess[ii]
    prop <- x[ii_lag] + sign(excess[ii]) * rmultinom_wrapper(1, abs(excess[ii]), bin_w)
    # possibly deal with negatives here
    x[ii_lag] <- prop
  }
  x
}

#' Form and combine Cartesian products of mappings encoded by rows of a data frame
#'
#' @param df a data frame encoding a multivariate mapping in a condensed form
#' @param ... a partition of the columns of the data frame into groups; each
#'   argument should either be a tidy-select argument indicating key columns in
#'   a group with no val columns, or a formula of the form <keyselector> ~
#'   <valselector>, where <keyselector> and <valselector> are tidy-select
#'   arguments
#' @return a data frame conceptually formed by this process: (1) every non-list
#'   column is converted to a list column with entries of length 1; (2) within
#'   each group of columns, each row should have entries of length 1 or the same
#'   non-1 number --- the former are \code{rep}'d to match the length of the
#'   latter; (3) for each row, take the Cartesian product between data frames
#'   formed from the column groups; (4) `bind_rows` to combine the Cartesian
#'   products together. Or alternatively, by simply calling \code{unchop} for
#'   each column group separately.
#'
#' @examples
#'
#' tbl1 <- tibble::tribble(
#'   ~location, ~time, ~unique_time_description,
#'   c("AK","AL"), c(as.Date("2021-01-01"), as.Date("2021-01-02"), as.Date("2021-01-03")), c("New Year's Day","Other","Other"),
#'   "AK", as.Date("2021-03-29"), "Seward's Day"
#' )
#' zookeeper:::unchop_cartesian_mapping(
#'   tbl1,
#'   location, time ~ unique_time_description
#' )
#'
#' ## In the above example, there must not be duplicate (location, time) values
#' ## in the result. In the below examples, duplicate (location, time) values
#' ## are allowed, but entirely duplicated rows are not.
#'
#' tbl2 <- tibble::tribble(
#'   ~location, ~time, ~time_tag,
#'   c("AK","AL"), as.Date("2021-03-29"), "Not New Year's",
#'   "AK", as.Date("2021-03-29"), "Seward's Day"
#' )
#' zookeeper:::unchop_cartesian_mapping(
#'   tbl2,
#'   location, starts_with("time")
#' )
#'
#' tbl3 <- tibble::tribble(
#'   ~time, ~location, ~tag,
#'   as.Date("2021-01-01"), c("AK","AL"), "New Year's Day",
#'   c(as.Date("2021-01-02"),as.Date("2021-01-03")), c("AK","AL"), "Other",
#'   as.Date("2021-03-29"), c("AK","AL"), c("Seward's Day","Other")
#' )
#' zookeeper:::unchop_cartesian_mapping(
#'   tbl3,
#'   time, location ~ tag
#' )
#'
#' @export
unchop_cartesian_mapping <- function(df, ...) {
  ## Translate `...` into list of list of (potentially named) integer vectors; `group_key_val_selections[[g]][[1L]]` should be the indices of the key selections of the `g`th group, and `group_key_val_selections[[g]][[2L]]` those of the vals:
  group_quosures <- rlang::enquos(...)
  group_key_val_selections <-
    purrr::map(rlang::enquos(...), function(group_quosure) {
      group_expr <- rlang::get_expr(group_quosure)
      group_env <- rlang::get_env(group_quosure)
      if (rlang::is_formula(group_expr)) {
        ## tidyselector for keys ~ tidyselector for vals
        if (length(group_expr) != 3L) {
          stop ('formula selectors must have both a LHS (keys) and RHS (vals)')
        }
        key_selections <- tidyselect::eval_select(rlang::new_quosure(group_expr[[2L]], group_env), df)
        val_selections <- tidyselect::eval_select(rlang::new_quosure(group_expr[[3L]], group_env), df)
      } else {
        ## tidyselector for keys (no vals)
        key_selections <- tidyselect::eval_select(group_quosure, df)
        val_selections <- integer(0L)
      }
      list(key_selections, val_selections)
    })
  ## Check that `...` encoded a partition of the columns:
  matched_col_is <- unlist(group_key_val_selections)
  if (length(matched_col_is) != ncol(df) || anyDuplicated(matched_col_is) != 0L) {
    unmatched_col_names <- names(df)[setdiff(seq_len(ncol(df)), matched_col_is)]
    duplicated_col_names <- names(df)[unique(matched_col_is[duplicated(matched_col_is)])]
    stop (sprintf('Each column of `df` must be matched exactly once;\n  Columns matched zero times: %s;\n  Columns matched multiple times: %s', toString(unmatched_col_names), toString(duplicated_col_names)))
  }
  ## Require at least one key column per group:
  if (any(vapply(group_key_val_selections, function(key_val_selection) length(key_val_selection[[1L]]), integer(1L)) == 0L)) {
    stop ('Each column group must contain at least one key column.')
  }
  partition <- lapply(group_key_val_selections, function(key_val_selection) c(key_val_selection[[1L]], key_val_selection[[2L]]))
  ## Calculate the result:
  result <- purrr::reduce(.init = df, .x = partition, .f = tidyr::unchop)
  ## Check that key-val relationships actually hold:
  key_col_is <- unlist(purrr::map(group_key_val_selections, 1L))
  duplicate_row_i <- anyDuplicated(result[key_col_is])
  if (duplicate_row_i  != 0L) {
    duplicate_row <- result[duplicate_row_i, key_col_is]
    stop (paste0('Duplicate composite key values found while unchopping; e.g., ', paste(names(duplicate_row), duplicate_row, sep="=", collapse=", ")))
  }
  return (result)
}

#' Add columns \code{manual_flag}, \code{manual_flag_max_lag} to \code{df} based on \code{manual_flags}
#'
#' @param df a data frame with at least the columns \code{data_source},
#'   \code{signal}, \code{geo_value}, \code{time_value}; must either contain
#'   only a single value for \code{data_source} and for \code{signal} or be
#'   grouped by these columns to make it appear that way
#' @param manual_flags a data frame with columns \code{data_source},
#'   \code{signal}, \code{geo_value}, \code{time_value}, and \code{max_lag}, to
#'   be expanded from a(n optional) compressed format using
#'   \code{\link{unchop_cartesian_mapping}} with grouping specification
#'   \code{data_source, signal, geo_value, time_value ~ max_lag}
#' @return \code{df} with \code{manual_flag} and \code{manual_flag_max_lag}
#'   columns added
make_manual_flags <- function(df, manual_flags) {
  stopifnot(! any(c("manual_flag", "manual_flag_max_lag") %in% colnames(df)))
  ds <- df$data_source[[1L]]
  sig <- df$signal[[1L]]
  stopifnot(all(df$data_source == ds), all(df$signal == sig))
  manual_flags <- unchop_cartesian_mapping(manual_flags, .data$data_source, .data$signal, .data$geo_value, time_value ~ .data$max_lag)
  manual_flags <- dplyr::filter(manual_flags,
                                 .data$data_source == ds, .data$signal == sig)
  manual_flags <- dplyr::mutate(manual_flags, manual_flag = TRUE)
  manual_flags <- dplyr::rename(manual_flags, manual_flag_max_lag = .data$max_lag)
  if (nrow(manual_flags) < 1) return(df)
  if (nrow(dplyr::anti_join(manual_flags, df, by=c("data_source","signal","geo_value","time_value"))) > 0L) {
    warning(sprintf('manual_flags marked entries of df that do not exist; for the current data_source and signal being inspected:\n%s',
                    paste(collapse="\n", utils::capture.output(print(manual_flags)), sep="\n")))
  }
  df <- dplyr::left_join(df, manual_flags, by=c("data_source","signal","geo_value","time_value"))
  df <- dplyr::mutate_at(df, dplyr::vars(.data$manual_flag), dplyr::coalesce, FALSE)
  return(df)
}

#' Make corrections to \code{df} as specified by \code{manual_flag} and \code{manual_flag_max_lag} columns
#'
#' @param df a data frame with at least the columns \code{data_source},
#'   \code{signal}, \code{geo_value}, \code{time_value}, \code{corrected},
#'   \code{fmedian}, \code{special_flag}, \code{manual_flag}, and
#'   \code{manual_flag_max_lag}; must either have a signal value for
#'   \code{data_source}, for \code{signal}, and for \code{geo_value}, or be
#'   grouped by these columns to make it appear that way
#' @return \code{df} with updated \code{corrected} and \code{special_flag}
#'   columns
make_manual_corrections <- function(df) {
  stopifnot(
    nrow(df) == 0L ||
    ("data_source" %in% dplyr::groups(df) || all(df[["data_source"]] == df[["data_source"]][[1L]])) &&
    ("signal" %in% dplyr::groups(df) || all(df[["signal"]] == df[["signal"]][[1L]])) &&
    ("geo_value" %in% dplyr::groups(df) || all(df[["geo_value"]] == df[["geo_value"]][[1L]]))
  )
  df <- dplyr::mutate(df,
                      corrected = corrections_multinom_roll(
                        .data$corrected, .data$corrected - .data$fmedian, .data$manual_flag, .data$time_value,
                        .data$manual_flag_max_lag
                      ),
                      special_flag = .data$special_flag | .data$manual_flag)
  return(df)
}
