## Packaged versions of code

## Manifest

1. README, this file
1. `template` A set of files used as a template to create a package
   for other forecasters. An example below shows how the `strawman`
   package can be built.
1. `strawman` the strawman versions 1 and 2 forecasters
   
## Introduction 

So far code has been run via scripts, but that quickly becomes
cumbersome in production environments. Hence the packaging approach.

As an example below, I have provided some templates to make it easier
to package your scripts. I am afraid there is no easy solution---some
parts require manual work.

## TO BE EDITED BELOW FOR NEW ORGANIZATION

## The steps

We will use the contents of the `strawman` directory under
   `covidcast-forecast/forecaster_code` as a way to illustrate how to
   wrap your R scripts into a package.

1. In an R session source the code in `make_pkg.R`. This will realize
   a function named `make_pkg`

```{r, eval = FALSE}
source("make_pkg.R")
```

2. Set the name of your package and provide a few details. You want to
   ensure that your current working directory does not contain a
   subdirectory called `strawman`. Pick a different directory in that
   case. 

```{r, eval = FALSE}
pkg_name <- "strawman"

## To make strawman package
make_pkg(pkg_name = pkg_name, first_name = "Samyak", last_name = "Rajanala",
         email = "samyak@stanford.edu")
```

This creates a package directory in your current working directory
named `strawman`. 


3. Copy the forecaster files into the `R` subdirectory of this package
   directory. 
   
```{r, eval = FALSE}
## Copy your forecaster function code
## Use path to covidcast repo local copy on your machine
## For me it is "~/GitHub/covidcast-forecast"
covidcast_path <- path("~", "GitHub", "covidcast-forecast")
file_copy(path = path(covidcast_path, "forecaster_code", "strawman", "strawman_forecaster.R"),
          new_path = pkg_r_path)
file_copy(path = path(covidcast_path, "forecaster_code", "strawman", "strawman_forecaster_helper.R"),
          new_path = pkg_r_path)
```

4. Create a new project in Rstudio using this directory. 

See video (2min30sec) I have uploaded to help. Oscar expected!

5. For each `library(pkg)` you had earlier, add a line `#' @import
   pkg`; see the file `strawman/R/strawman-package.R` as an
   example. Note that importing `tidyverse` is not advised. Instead,
   you will mostly only need to import a few packages in `tidyverse`
   such as `dplyr`, `stringr`, `readr`, `purrr`, `magrittr`, `tibble`,
   `tidyr`, `ggplot2`. 
   
6. Examine the `get_forecasters.R` file (_note the plural_; this is a
   change )and modify the return value appropriately as shown in the
   comments. Example packages to emulate, in order of complexity:
   `strawman`, `poiszero`, `aardvark`. More on complexity below.

7. As shown in the video, run the `devtools::document()` again to
   regenerate the namespace file. There is no harm in doing this a few
   times to ensure that old definitions get rewritten; indeed, it is
   necessary sometimes to do so, by getting out of Rstudio and getting
   back in again. 

8. To build the package, in Rstudio use

```{r, eval = FALSE}
devtools::build()
```

At this point, with some luck, you will have something that looks like
a package, if you got all your imports in step 5 correct. But
conflicts often arise which requires you to selectively import
functions as discussed below. The resulting `tar.gz` file can be
installed like any other package.


## Multiple forecasters in one package

The first iteration of this process assumed each forecaster would be
in its own package. This meant that if one has two versions of a
forecaster, with minor changes to some internal parameters, one would
have two packages doubling the maintenance effort. So, the current
version has a function `get_forecasters()` (plural) rather than the
singular `get_forecaster()` in the first iteration. Note also the five
parameters that will always be passed to `get_forecasters()`: you have
them, whether you choose to use them or not.

- `response` the response (e.g. `"usafacts_deaths_incidence_num"`)
- `incidence_period` the incidence period (e.g. `"epiweek"` for
     now, for all forecasters)
- `ahead` the ahead parameter (e.g. 1, 2, 3, 4)
- `forecast_date` the date of the forecast
- `geo_type` the geographic type (e.g `"county"` or `"state"` or
`"hrr"` or `"msa"` but for now only the first two
- `n_locations` the number of locations (default 200)

The return value should be a _named_ list of lists, with each list
element consisting of two elements: a forecaster function and the type
of the forecaster. See example below. If there is some combination of
parameters that is not addressed by the forecaster, then the
forecaster function should be `NA` rather than a function; this is
used to detect whether to run it or not.

The `type` argument should be one of `standalone` or `ensemble`, so
that the harness can schedule the ensemble methods to run after all
stand alone forecasters can be run. 

### Example

Suppose you have five incarnations of your forecaster. All versions
work off the same code base except with different values for some
other variables besides those passed to this `get_forecasters()`
function, or maybe even a few other tweaks.

Take for example:
- incarnation 1 (the default) uses parameters `weeks_back = 3` and `solver = "gurobi"`
- incarnation 2 uses parameters `weeks_back = 2` and `solver = "gurobi"`
- incarnation 3 uses parameters `weeks_back = 3` and `solver = "Rglpk"`
- incarnation 4 uses parameters `weeks_back = 2` and `solver = "Rglpk"`
- incarnation 5 is an ensemble method that uses the results of all
  `standalone` forecasters with `solver = NA`.

Let's give codenames for these: `fisher`, `bayes`, `mle`, `mcmc` and
`ebayes` respectively.  Then you should return a list such as this
where `my_forecaster` is your forecaster function.

```{r, eval = FALSE}
list(
    fisher = list(forecaster = my_forecaster(response = response, incidence_period = incidence_period,
                                             ahead = ahead, forecast_date = forecast_date,
                                             geo_type = geo_type, n_locations = n_locations,
                                             weeks_back = 3, solver = "gurobi"),
                  type = "standalone"),
    bayes = list(forecaster = my_forecaster(response = response, incidence_period = incidence_period,
                                            ahead = ahead, forecast_date = forecast_date,
                                            geo_type = geo_type, n_locations = n_locations,
                                            weeks_back = 2, solver = "gurobi"),
                 type = "standalone"),
    mle = list(forecaster = my_forecaster(response = response, incidence_period = incidence_period,
                                          ahead = ahead, forecast_date = forecast_date,
                                          geo_type = geo_type, n_locations = n_locations,
                                          weeks_back = 2, solver = "Rglpk"),
                 type = "standalone"),
    mcmc = list(forecaster = my_forecaster(response = response, incidence_period = incidence_period,
                                           ahead = ahead, forecast_date = forecast_date,
                                           geo_type = geo_type, n_locations = n_locations,
                                           weeks_back = 3, solver = "Rglpk"),
                 type = "standalone"),
    ebayes = list(forecaster = my_forecaster(response = response, incidence_period = incidence_period,
                                             ahead = ahead, forecast_date = forecast_date,
                                             geo_type = geo_type, n_locations = n_locations,
                                             weeks_back = 3, solver = NA),
                  type = "ensemble")
)
```

More sophisticated checks are possible. You may have some forecasters
that only work for certain `ahead`s; say, `bayes` only works if only
if `(ahead > 1 && ahead < 4)`. The the entry for `bayes` in the list
above can be modified to _not to return a function_ thus:

```{r, eval = FALSE}
bayes = if (ahead <= 1 || ahead >= 4) {
            list(forecaster = NA, type = "standalone") ## forecaster not a function!
        } else {
            list(forecaster = my_forecaster(response = response, incidence_period = incidence_period,
                                            ahead = ahead, forecast_date = forecast_date,
                                            geo_type = geo_type, n_locations = n_locations,
                                            weeks_back = 2, solver = "gurobi"),
                 type = "standalone")
        }

```

You also have the ability to pass further additional parameters to
your raw functions, as you see fit. This is used in `strawman`, for
example, where one function does everything and only variant is the version.
It also doesn't use all the arguments we have available, so it returns:
```{r}
list(
    strawman = list(
        forecaster = strawman_forecaster(response = response, incidence_period = incidence_period,
                                         ahead = ahead, version = 2),
        type = "standalone"),
    strawman1 = list(
        forecaster = strawman_forecaster(response = response, incidence_period = incidence_period,
                                         ahead = ahead, version = 1),
        type = "standalone")
)
```

## How to test your forecaster

Use the function `evalforecast::run_forecast()` to test any forecaster
you build, whether you have it in a package or not. The example below
assumes it is in a package, but you can just as well use a function
your session as `my_forecaster` in the code below. 

Let's build a simple template for testing the `strawman` forecaster,
version 2. 

```{r, eval = FALSE}
library(evalforecast)

## Where the data is
data_path  <- "~/GitHub/delphipkg/upstream_df_county_2020-06-21_v3_final.Rdata"

## Set forecaster parameters
response <- "usafacts_deaths_incidence_num" # variable we are forecasting
incidence_period <- "epiweek"
ahead <- 1
n_locations <- 200
geo_type  <- "county"
forecast_date <- lubridate::ymd("2020-06-22")

## Get your forecasters ready
forecasters <- strawman::get_forecasters(response = response,
                                         incidence_period = incidence_period,
                                         ahead = ahead,
                                         forecast_date = forecast_date,
                                         geo_type = geo_type,
                                         n_locations = 200)
## Choose the strawman forecaster
my_forecaster  <- forecasters[["strawman"]][["forecaster"]]

## make predictions
predictions <- run_forecaster(response = response,
                              incidence_period = incidence_period,
                              ahead = ahead,
                              forecast_date = forecast_date,
                              geo_type = geo_type,
                              n_locations = 200,
                              ## two additional params
                              data_path = data_path,
                              forecaster = my_forecaster)

```

If your forecaster works in the above setting, there's a very high
likelihood that it will work in the production setting too!  _However,
for the production setting, you really should test after putting your
forecaster in a package!_


### How is this infrastructure used?

The production process basically uses `run_forecaster` on all
available forecasters, i.e., those that have been installed as
packages. It has a list of installed forecaster packages and just
runs through a loop for each package. In fact, here is the production
script for running the _non ensemble_ forecasters. 

```{r, eval = FALSE}
library(logger)
library(sessioninfo)
library(fs)

e  <- new.env(parent = emptyenv())
log_info("Loading county data")
load("upstream_df_county_2020-06-21_v3_final.Rdata", envir = e)
e$data_list <- list(county = e$df)
log_info("Loading state data")
load("upstream_df_state_2020-06-21_v3_final.Rdata", envir = e)
e$data_list$state <- e$df

## these variables specify the forecasting task:
responses <-  list(county = "usafacts_deaths_incidence_num", state = "jhu-csse_deaths_incidence_num")

## Forecasting packages we use
forecasting_packages  <- c("strawman", "poiszero", "aardvark", "stackedLasso")
output_path  <- "~/GitHub/covidcast-forecast/forecaster_predictions"
##output_path  <- "~/GitHub/delphipkg/forecaster_predictions"

## Output structure
## - forecast_date/ahead/response/geo_type/incidence_period/n_locations/forecaster/out.RDS

## Main prediction code
incidence_period <- "epiweek"
n_locations <- 200
forecast_date <- lubridate::ymd("2020-06-22") # this would be changed to today()

for (ahead in 1:4) {
    for (geo_type in c("county", "state")) {
        response <- responses[[geo_type]]
        all_offerings <- purrr::flatten(
                                    purrr::map(.x = forecasting_packages,
                                               .f = function(pkg) {
                                                   utils::getFromNamespace(x = "get_forecasters", ns = pkg)(
                                                       response = response,
                                                       incidence_period = incidence_period,
                                                       ahead = ahead,
                                                       geo_type = geo_type,
                                                       n_locations = n_locations,
                                                       forecast_date = forecast_date)
                                               })
                                )
        non_ensembles <- all_offerings[sapply(all_offerings, function(x) x$type != "ensemble")]
        non_ensemble_forecasters  <- lapply(non_ensembles, function(x) x$forecaster)
        result  <- evalforecast::run_forecasters(forecasters = non_ensemble_forecasters,
                                                 response = response,
                                                 incidence_period = incidence_period,
                                                 ahead = ahead,
                                                 geo_type = geo_type,
                                                 n_locations = n_locations,
                                                 forecast_date = forecast_date,
                                                 data_list = e$data_list)

        logger::log_info("Saving prediction outputs")
        for (fn in names(result)) {
            output_path  <- fs::path(forecast_date, ahead, response, geo_type, incidence_period, n_locations, fn)
            if (!fs::dir_exists(output_path)) fs::dir_create(output_path, recurse = TRUE)
            saveRDS(result[[fn]], file = fs::path(output_path, "out.RDS"))
        }
        logger::log_info("Done.")
    }
}

sessioninfo::session_info()

```

That's all.

## Notes

Many of the scripts use library statements one after another.  And
some of them generate conflict messages. These can be _dangerous_: you
want to be sure that function `foo` that you are calling is exactly
from package `bar` as you intended.
   
The only sure of way making that possible is not use the `library`
function, but rather import the package. Importing loads the package
but doesn't put in your search path. The ideal approach is to use the
construct `bar::foo(x, y)` for every call you make to function `foo`
in package `bar` and selectively import the functions you actually
use. This can quickly become a chore.
   
A reasonable first approximation is to import all functions in a
package hoping that there are no conflicts. If there are, then you
have no choice but to resolve them. This is the approach I have taken
since we don't have infinite time. When I find a conflict where two
packages `bar` and `foobar` provide function `foo` I selectively
import only needed functions from one of them.
   
As you can tell, I am not always successful: `stackedLasso` is still
not working. 

## Help

I can help you get over the finish line if you get somewhere with the
above steps.



