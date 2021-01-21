make_kernel_smoother <- function(h, kernel = c("tophat"), first_date = NULL, last_date = NULL){
  # Closure to make a smoother.
  # Inputs:
  # -- h: bandwidth (number of trailing days to smooth over)
  # -- kernel: name of smoothing kernel to use
  # -- first_date: Date object, the first time_value on which we should see variables.
  # -- last_date: Date object, the last time_value on which we should see variables.
  
  kernel <- match.arg(kernel)
  
  if (is.null(first_date)){
    first_date <- min(dat %>% pull(time_value))
  }
  if (is.null(last_date)){
    last_date <- max(dat %>% pull(time_value))
  }

  if ( kernel == "tophat" ){
    kernel_smoother <- function(dat){
      
      date_df <- data.frame(time_value = seq(first_date, last_date, by = "days"))
      full_df <- left_join(date_df, dat, by = c("time_value"))
      
      smoothed_dat <- full_df %>%
        group_by(location) %>%
        arrange(time_value) %>%
        mutate(smoothed_value = rollmean(value, h, align = "right", fill = "extend")) %>%
        ungroup
      return(smoothed_dat)
    }
  }
}