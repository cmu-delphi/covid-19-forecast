make_stratifier_by_n_responses <- function(alpha){
  stratify_by_n_responses <- function(df_use, response){
    # Stratify locations into two categories: 'more grim' locations vs. 'less grim' locations.
    # --'more grim' locations are those that are in the top (1 - alpha) * 100% in terms
    # of their cumulative count of the response variable (usually deaths).
    # --'less grim' locations are the rest.
    #
    # Input:
    #  df_use: data frame with at least column names location, value, variable_name
    #  response: character, what variable to treat as response
    #  alpha: numeric, we will take 1 - alpha locations to be 'more grim' locations.
    # Output:
    # df_strata: data frame with location and strata column (Boolean) -- TRUE for 'more grim'
    # and FALSE for 'less grim'

    df_response <- df_use %>% filter(variable_name == !!response)

    df_strata <- df_response %>%
      group_by(location, variable_name) %>%
      summarize(n_response = sum(value), .groups = "drop") %>%
      mutate( strata = (n_response >= quantile(n_response, alpha)) ) %>%
      select(location, strata)
  }
}
