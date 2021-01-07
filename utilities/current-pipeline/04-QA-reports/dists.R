
read_pred_file <- function(forecaster){
  all_tb <- tibble(NULL)
  for (ahead in 1:4) {
    target_period <- evalforecast::get_target_period(forecast_date,"epiweek",ahead)
    data_loc <- file.path(forecast_dir,ahead,response,geo,dvar_type,dvar,forecaster,"out.RDS")
    pred_tb <- readRDS(data_loc) %>% 
      add_column(ahead = ahead, target_start_date = target_period$start) %>%
      unnest(cols = c("forecast_distribution"))%>%
      left_join(fips_names, by=c("location"))
    all_tb <- bind_rows(all_tb, pred_tb)
  }
  return(all_tb)
}

comb_pred <- function(all_tb){
  red_data <- rec_data %>% filter(location %in% unique(all_tb$location))
  great_tb <- bind_rows(all_tb,red_data)
  return(great_tb)
}

plot_traj <- function(all_tb,lower_p,upper_p,title_pre){
  plot_ret <- all_tb %>% ggplot(aes(x=target_start_date,y=probs_0.5,ymin=!!lower_p,ymax=!!upper_p)) + 
    geom_line(col='red') + 
    geom_ribbon(alpha=0.2) + facet_wrap(~location_name,ncol = 5,scales="free_y") + 
    geom_line(mapping = aes(x=reference_date, y=value_roll),col='black') +
    ggtitle(paste(title_pre,sprintf("\nForecaster: %s",forecaster))) + 
    ylab('quantile') + xlab('date') 
  return(plot_ret)
}

all_tb <- read_pred_file(forecaster)
all_tb_fac <- all_tb %>% mutate(ahead = factor(ahead), probs = factor(probs))

probs <- unique(all_tb$probs)
stat_df <- tibble(NULL)
iqr_var_names <- c()

for (i in 1:11) {
  q1 <- probs[i]
  q2 <- probs[24-i]
  iqr_var_name <- sprintf("iqr_%s", q2 - q1)
  iqr_var_names <- c(iqr_var_names,iqr_var_name)
  q1_var_name <- as.symbol(sprintf("probs_%s", q1))
  q2_var_name <- as.symbol(sprintf("probs_%s", q2))
  
  stat_df_temp <- all_tb %>% filter(probs %in% c(q1,q2)) %>% 
    spread(probs,quantiles, sep='_') %>%
    mutate(stat_name=iqr_var_name, value=!!q2_var_name - !!q1_var_name) %>%
    select(location, ahead, stat_name, value)
  stat_df <- bind_rows(stat_df, stat_df_temp)
}


plot_width <- 10
plot_height <- 8





