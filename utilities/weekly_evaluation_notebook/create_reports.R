## Start of prologue
## These are usually not changed except by those know what is going
## on.  Volatile packages are those that undergo frequent development
## and are therefore installed just-in-time from repos before running
## forecasts

## Install a package if not already installed from github
install_from_github_if_needed <- function(pkgs, ...) {
  installed_pkgs <- installed.packages()[, 1]
  pkg_names <- names(pkgs)
  to_install <- pkgs[setdiff(pkg_names, installed_pkgs)]
  for (pkg in to_install) {
    devtools::install_github(repo = pkg$repo, ref = pkg$ref,
                             subdir = pkg$subdir, ...)
  }
}

volatile_pkgs <- list(
  evalcast = list(repo = "cmu-delphi/covidcast", ref = "evalcast",
                   subdir = "R-packages/evalcast")
)

install_from_github_if_needed(volatile_pkgs, upgrade = "never")

## End of prologue
