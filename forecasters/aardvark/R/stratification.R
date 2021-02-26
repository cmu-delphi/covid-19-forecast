make_stratifier_by_n_responses <- function(alpha = 0.5){
  stratify_by_n_responses <- function(df_use, response){

    df_response <- df_use %>% filter(variable_name == !!response)
    df_strata <- df_response %>%
      group_by(location, variable_name) %>%
      summarize(n_response = sum(value), .groups = "drop") %>%
      mutate( strata = (n_response >= quantile(n_response, alpha)) ) %>%
      select(location, strata)
  }
}
