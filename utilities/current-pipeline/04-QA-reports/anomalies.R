## 0's

# all_tb <- read_pred_file(forecaster)
all_tb_wide <- all_tb %>% spread(probs,quantiles,sep="_")
comb_tb <- comb_pred(all_tb_wide)

for (i in 1:6){
  q1 <- probs[i]
  q2 <- probs[24-i]
  q1_var_name <- as.symbol(sprintf("probs_%s", q1))
  q2_var_name <- as.symbol(sprintf("probs_%s", q2))
  
  zero_locs <- stat_df %>% filter(stat_name == iqr_var_names[i], value == 0)
  zero_locs <- unique(zero_locs$location)
  if (length(zero_locs) > 0) {
    zero_rows <- comb_tb %>% filter(location %in% zero_locs)
    plot_fname <- sprintf("zeros_%s_%s.pdf",iqr_var_names[i],forecaster)
    if (length(zero_locs) > 4) { 
      plot_width <- 10 
    } else { 
      plot_width <- max(length(zero_locs) * 2, 4) 
    }
    plot_height <- 0.5+1.5*ceiling(length(zero_locs) / 5)
    print(plot_traj(zero_rows,q1_var_name,q2_var_name,paste("Trajectories with zero IQR @",iqr_var_names[i])))
  }
}



for (i in 1:6){
  q1 <- probs[i]
  q2 <- probs[24-i]
  q1_var_name <- as.symbol(sprintf("probs_%s", q1))
  q2_var_name <- as.symbol(sprintf("probs_%s", q2))
  
  stat_red <- stat_df %>% filter(stat_name == iqr_var_names[i])
  val_loc <- stat_red %>% group_by(location) %>% summarize(max_iqr = max(value)) %>% top_n(10)
  top_locs <- unique(val_loc$location)

  if (length(zero_locs) > 0) {
    top_rows <- comb_tb %>% filter(location %in% top_locs)
    
    plot_fname <- sprintf("top_%s_%s.pdf",iqr_var_names[i],forecaster)
    if (length(top_locs) > 4) { 
      plot_width <- 10 
    } else { 
      plot_width <- max(length(top_locs) * 2, 4) 
    }
    plot_height <- 0.5+1.5*ceiling(length(top_locs) / 5)
    print(plot_traj(top_rows,q1_var_name,q2_var_name,paste("Trajectories with top IQR @",iqr_var_names[i])))
  }
}

