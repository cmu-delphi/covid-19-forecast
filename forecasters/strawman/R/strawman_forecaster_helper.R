#' @importFrom stats quantile
strawman_single_region_quantiles <- function(data,
                                             increase,
                                             target_epiweek,
                                             probs,
                                             version,
                                             ahead) {

  # This should filter only the partial current epiweek out.
  data <- data %>% filter(epiweek <= target_epiweek - ahead)

  response <- data %>% pull(value)
  train_epiweeks <- data %>% pull(epiweek)



  most_recent_epiweek <- max(train_epiweeks)
  if(target_epiweek != most_recent_epiweek + ahead)
    warning("Not using most recent epiweek")

  y <- response[which.max(train_epiweeks)]

  if(version == 1){

    empirical <- y + increase

  } else if(version == 2){

    vector_size <- length(increase) ** ahead
    if(vector_size > 10000) stop("For version 2, we will probably want a better way of convolution.")

    z <- 0

    for(i in 1:ahead){
      z <- rowSums(expand.grid(z, increase))
    }

    empirical <- y + z

  } else{
    stop("Invalid version.
         However, you should never have been able to see this message.
         Contact Samyak.")
  }


  quantiles <- pmax(0, stats::quantile(empirical, probs))

  return(list(
    output = data.frame(
      probs = probs,
      quantiles = quantiles
    ),
    diagnostics = list(
    )
  ))

}




strawman_quantiles_all_regions <- function(data,
                                           target_epiweek,
                                           probs,
                                           version,
                                           ahead) {

  mydata <- data

  unique_locations <- unique(mydata$location)

  output_dataframe <- data.frame()

  for (i in 1:length(unique_locations)) {

    current_location <- unique_locations[i]

    current_location_indices <-
      (mydata$location == current_location)

    current_location_y <-
      pull(mydata, value)[current_location_indices]


    if(!all(is.na(current_location_y))){
      output_prediction <- strawman_single_region_quantiles(
        data = mydata %>%
          filter(location == current_location) %>%
          select(-location, -increase),
        increase = mydata %>%
          filter(location == current_location) %>%
          pull(increase),
        target_epiweek = target_epiweek,
        probs = probs,
        version = version,
        ahead = ahead
      )

      current_location_output <- output_prediction$output
    } else{
      current_location_output <- cbind(probs = probs,
                                       quantiles = NA)
    }



    output_dataframe <- rbind(output_dataframe,
                              cbind(location = current_location,
                                    current_location_output))
  }

  output_dataframe <- mutate(output_dataframe,
                             probs = as.numeric(as.character(probs)),
                             quantiles = as.numeric(as.character(quantiles)))

  return(output_dataframe)
}




