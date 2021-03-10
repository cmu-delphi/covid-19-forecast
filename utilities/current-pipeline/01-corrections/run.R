library(magrittr)
source("corrections.R")

## Get the current corrections data for state
get_data_corrections(db_path = "./data_corrections.sqlite", geo_type = "state")

## Get the current corrections data for county
get_data_corrections(db_path = "./data_corrections.sqlite", geo_type = "county")

## Apply corrections to state data
e  <- new.env()
load("~/Documents/upstream_dfs/upstream_df_state_2020-09-06_final.Rdata", envir = e)
new_df <- apply_corrections(df = e$df, geo_type = "state", "./data_corrections.sqlite")
nrow(e$df) == nrow(new_df)  # Same number of rows?


## The example below is just a check to show no corrections available
## for county.  One should never mix up county data and state data! A
## check would be good to add.
new_df <- apply_corrections(df = e$df, geo_type = "county", "./data_corrections.sqlite")

## Update corrections, but first make a copy
file.copy(from = "./data_corrections.sqlite", to = "./test.sqlite")
d <- get_data_corrections(db_path = "./test.sqlite", geo_type = "state")  ## 21-row tibble
## Delete all rows but the first five
d <- d[1:5, ]
update_corrections("./test.sqlite", "state", d)
get_data_corrections(db_path = "./test.sqlite", geo_type = "state")  ## 5-row tibble
## Modify d by adding a column
d$foo <- 1
update_corrections("./test.sqlite", "state", d) ## should fail since schema doesn't match

