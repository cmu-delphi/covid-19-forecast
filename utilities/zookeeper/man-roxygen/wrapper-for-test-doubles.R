#' @section Wrapper functions:
#'
#' Wrapper functions for base/external functions enable
#' (easier/cleaner/wider/any) unit testing with test doubles. Current methods
#' for using test doubles without built-in dependency injection include:
#' - [`mockr::with_mock`], which is restricted to functions in the package under test
#' - [`testthat::with_mock`], which is deprecated (and restricted to non-base functions)
#' - [`mockery::stub`], which is a "shallower" replacement compared to the `with_mock` methods
#'
#' @md
