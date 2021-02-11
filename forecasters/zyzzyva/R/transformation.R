## functions for transformation go here
## all functions should be named: `tr.function_name()`

#' Identity transformation
#'
#' @param x the vector to be transformed
#' @return transformed vector
#' @export tr.identity
tr.identity <- function(x) {
  x
}

#' Padded log transformation for count data
#'
#' @param x the vector to be log transformed
#' @param a the scalar constant to pad the vector, elementwise
#' @param clip_left_0 boolean to clip negative values to 0
#' @return transformed vector
#' @export tr.log_pad
tr.log_pad <- function(x, a = 1, clip_left_0 = TRUE) {
  if (clip_left_0)
    x[x < 0] <- 0

  log(a + x)
}

#' Inverse padded log transformation for count data
#'
#' @param x the vector to be inversely log_pad transformed
#' @param a the scalar constant to subtract from the vector, elementwise
#' @return inversely transformed vector
#' @export tr.inv_log_pad
tr.inv_log_pad <- function(x, a = 1) {
  exp(x) - a
}

#' Cube root transformation
#'
#' @param x the vector to be cube root transformed
#' @return transformed vector
#' @export tr.cube_root
tr.cube_root <- function(x) {
  sign(x) * abs(x) ^ (1 / 3)
}

#' Column mean imputation
#'
#' @description Performs mean imputation on a matrix.  If all NAs, impute to zero.
#'
#' @param matx the matrix
#' @param impute_columns the columns to impute, default all
#' @export tr.column_impute
tr.column_impute <- function(matx, impute_columns = 1:ncol(matx)) {
  for (cidx in impute_columns) {
    missing = is.na(as.numeric(matx[, cidx]))
    matx[missing, cidx] = mean(as.numeric(matx[!missing, cidx]))
    if (sum(!missing) == 0) {
      matx[missing, cidx] = 0
    }
  }
  matx
}
