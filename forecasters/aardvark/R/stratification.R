# Stratifiers are functions which tell us what type of model to fit to each location.
# They always output a data frame

make_stratifier_by_n_responses <- function(alpha){
  stratify_by_n_responses <- function(df_use, response){
    # Stratify locations into two categories: 'good' signal locations vs. 'bad' signal locations.
    # --'Good' signal locations are those which the top 1 - alpha of all locations in terms
    # of magnitude of the response variable, summed across all time periods.
    # --'Bad' signal locations are the rest.
    #
    # Input:
    #  df_use: data frame with at least column names location, value, variable_name
    #  response: character, what variable to treat as response
    #  alpha: numeric, we will take 1 - alpha locations to be 'good' signal locations.
    # Output:
    # df_strata: data frame with location and strata column (Boolean) -- TRUE for 'good'
    # and FALSE for 'bad'

    df_response <- df_use %>% filter(variable_name == !!response)

    df_strata <- df_response %>%
      group_by(location, variable_name) %>%
      summarise(n_response = sum(value)) %>%
      ungroup %>%
      mutate( strata = (n_response >= quantile(n_response, alpha)) ) %>%
      select(location, strata)
  }
}