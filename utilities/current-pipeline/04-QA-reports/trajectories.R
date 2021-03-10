
read_pred_file <- function(forecaster){
  all_tb <- tibble(NULL)
  for (ahead in 1:4) {
    target_period <- evalforecast::get_target_period(forecast_date,"epiweek",ahead)
    target_start <- target_period$end
    data_loc <- file.path(forecast_dir,ahead,response,geo,dvar_type,dvar,forecaster,"out.RDS")
    
    pred_tb <- readRDS(data_loc)
    pred_tb <- pred_tb %>% 
      add_column(target_start_date = target_period$start, target_end_date = target_period$end) %>%
      unnest(cols = c("forecast_distribution")) %>%
      filter(probs %in% c(0.2, 0.5, 0.8)) %>%
      spread(probs,quantiles,sep="_") 
    all_tb <- bind_rows(all_tb, pred_tb)
  }
  red_data <- rec_data %>% filter(location %in% unique(all_tb$location)) 
  all_tb <- bind_rows(all_tb,red_data) %>% select(-location_name)
  all_tb <- all_tb %>% left_join(loc_names, by=c("location"))
  return(all_tb)
}

plot_traj <- function(all_tb, facet_page = 1){
  #plot_fname <- sprintf("trajectory_plots_%s_%s.pdf",forecaster,facet_page)
  #plot_fpath <- file.path(out_dir,plot_fname)
  
  fac_plt <- all_tb %>% 
    ggplot(aes(x=target_start_date,y=probs_0.5,ymin=probs_0.2,ymax=probs_0.8)) + 
    geom_line(col='red') + 
    geom_ribbon(alpha=0.2) + 
    facet_wrap_paginate(~location_name,nrow = 10,ncol = 5,scales="free_y",page=facet_page) + 
    geom_line(mapping = aes(x=reference_date, y=value_roll),col='black') +
    ggtitle(paste(forecast_date,response,geo,dvar_type,dvar,sprintf("\nForecaster: %s",forecaster))) + 
    ylab('quantile') + xlab('date') +
    theme(text = element_text(size = 8))
    # ggsave(plot_fpath, width = plot_width, height = plot_height)
  stop_flag <<- n_pages(fac_plt) > facet_page
  fac_plt
}

all_tb <- read_pred_file(forecaster)
facet_page <- 0
stop_flag <- TRUE
while (stop_flag) {
  facet_page <- facet_page + 1
  tryCatch(
    print(plot_traj(all_tb, facet_page = facet_page)), 
    error = function(err) print(paste0("Ugh: ", err))
      )
}

