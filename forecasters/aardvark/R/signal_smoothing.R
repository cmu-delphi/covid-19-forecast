make_kernel_smoother <- function(h = 7, kern = "boxcar"){
  # Inputs:
  # -- h: bandwidth (number of trailing days to smooth over)
  # -- kernel: name of smoothing kernel to use
  
  kern <- match.arg(kern)

  if ( kern == "boxcar" ){
    
    kernel_smoother <- function(dat){
      
      first_date <- min(dat %>% pull(time_value))
      last_date <- max(dat %>% pull(time_value))
      date_df <- data.frame(time_value = seq(first_date, last_date, by = "days"))
      full_df <- left_join(date_df, dat, by = c("time_value"))
      
      smoothed_dat <- full_df %>%
        group_by(geo_value) %>%
        arrange(time_value) %>%
        mutate(smoothed_value = rollmean(value, h, align = "right", fill = "extend")) %>%
        ungroup
      
      return(smoothed_dat)
    }
  }
  
  return(kernel_smoother)
}
