if (!require(tidyverse)) stop("Please install tidyverse from CRAN")
if (!require(fs)) stop("Please install fs from CRAN")
if (!require(devtools)) stop("Please install devtools from CRAN")
if (!require(desc)) stop("Please install desc from CRAN")

#' Make a new package directory based on our template
#' @param pkg_name the name of the package (string)
#' @param first_name your first name
#' @param last_name your last name
#' @param email your email address
make_pkg  <- function(pkg_name, first_name, last_name, email) {
    fs::dir_copy("template", pkg_name)
    fs::path(pkg_name, "README.md") %>%
        read_lines() %>%
        gsub(pattern = "template", replacement = pkg_name) %>%
        write_lines(path = fs::path(pkg_name, "README.md"))
    fs::path(pkg_name, "DESCRIPTION") %>%
        read_lines() %>%
        gsub(pattern = "template", replacement = pkg_name) %>%
        gsub(pattern = "first_name", replacement = first_name) %>%
        gsub(pattern = "last_name", replacement = last_name) %>%
        gsub(pattern = "email_address", replacement = email) %>%
        write_lines(path = fs::path(pkg_name, "DESCRIPTION"))
    fs::path(pkg_name, "R", "template-package.R") %>%
        read_lines() %>%
        gsub(pattern = "template", replacement = pkg_name) %>%
        gsub(pattern = "first_name", replacement = first_name) %>%
        gsub(pattern = "last_name", replacement = last_name) %>%
        gsub(pattern = "email", replacement = email) %>%
        write_lines(path = fs::path(pkg_name, "R", paste0(pkg_name, "-package.R")))
    file_delete(fs::path(pkg_name, "R", "template-package.R"))
    fs::path(pkg_name, "tests", "testthat.R") %>%
        read_lines() %>%
        gsub(pattern = "template", replacement = pkg_name) %>%
        write_lines(path = fs::path(pkg_name, "tests", "testthat.R"))
    pkg_version  <- format(as.POSIXlt(Sys.time()), format="%Y%m%d-%H%M%S")
    d  <- desc::desc(file = fs::path(pkg_name, "DESCRIPTION"))
    d$set("Version", pkg_version)
    d$write(file = fs::path(pkg_name, "DESCRIPTION"))
}

#' Update the package version to be the current time stamp
#' @return the new package version as a string
#' @examples
#' ## Suppose package sources are in ~/tmp/strawman
#' update_pkg_version("~/tmp/strawman") ## will set new version
update_pkg_version <- function(pkg_path) {
    pkg_version  <- format(as.POSIXlt(Sys.time()), format="%Y%m%d-%H%M%S")
    d  <- desc::desc(file = fs::path(pkg_path, "DESCRIPTION"))
    d$set("Version", pkg_version)
    d$write(file = fs::path(pkg_path, "DESCRIPTION"))
    pkg_version
}