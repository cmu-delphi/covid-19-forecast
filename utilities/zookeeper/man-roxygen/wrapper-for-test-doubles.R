#' Wrapper functions for base/external functions enable
#' (easier/cleaner/wider/any) unit testing with test doubles. Current methods
#' for using test doubles without dependency injection include:
#' - [mockr::with_mock], which is restricted to functions in the package under test
#' - [testthat::with_mock], which is deprecated (and restricted to non-base functions)
#' - [mockery::stub], which appears to have issues with nested functions
#'
#' The [with_test] functions are restricted due to potential issues with bytecode and/or JIT compilation of base/external packages and/or cleanliness.  The [stub] function approach has issues due to the complexity of code and environment transformation.
#'
#' @md
