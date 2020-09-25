
fn_rolling_stocks <- function(df_input, payoffname, controls_levels, controls_logs, window_T, month_fe){
  
  permno_list <- df_input %>% group_by(permno) %>% count()
  n_permno    <- permno_list %>% nrow() %>% as.numeric()
  
  n_outputs <- 17
  
  output <- c()
  
  for (i in 1:n_permno) {
    
    per       <- permno_list$permno[i]
    df_permno <- df_input %>% filter(permno == per)
    
    T_periods <- df_permno %>% count() %>% pull() %>% as.numeric() # Total observations
    T_rolling <- T_periods - window_T + 1        # Number of iterations
    
    output_i <- matrix(ncol = n_outputs, nrow = T_rolling) # Empty data frame to store results
    
    print(paste0("rolling ", i," of " , n_permno))
    
    for (j in 1:T_rolling) {
      #print(j)
      df_window <- df_permno %>% slice(j:(j + window_T - 1)) ## Selecting relevant subsample
      
      recovered_list <- fn_identifying_stocks(df_window, payoffname, controls_levels, controls_logs, month_fe)
      
      output_i[j, 1:n_outputs] <- unlist(recovered_list, use.names = FALSE)
      
    }
    
    output <- rbind(output, output_i)
    
    }
  
  output <- data.frame(output)
  colnames(output) <- c("year_start", "month_start", "year_end", "month_end", 
                        "permno", "n_obs", "n_factors", 
                        "R2_long", "R2_short", "beta_0", "beta_1", "zeta_0", "var_eps", 
                        "out_long", "out_short", "tau_pi", "tau_pi_R")
  
  return(output)
  
}

fn_rolling_stocks_par <- function(df_input, payoffname, controls_levels, controls_logs, window_T, month_fe){
  
  permno_list <- df_input %>% group_by(permno) %>% count()
  n_permno    <- permno_list %>% nrow() %>% as.numeric()
  
  n_outputs <- 17
  
  results <- foreach(i = 1:n_permno, .combine = "rbind", .export = c('fn_identifying_stocks'), .packages = 'tidyverse') %dopar% {
    
    per       <- permno_list$permno[i]
    df_permno <- df_input %>% filter(permno == per)
    
    T_periods <- df_permno %>% count() %>% pull() %>% as.numeric() # Total observations
    T_rolling <- T_periods - window_T + 1        # Number of iterations
    
    output_i <- matrix(ncol = n_outputs, nrow = T_rolling) # Empty data frame to store results
    
    #print(paste0("rolling ", i," of " , n_permno))
    
    for (j in 1:T_rolling) {
      #print(j)
      df_window <- df_permno %>% slice(j:(j + window_T - 1)) ## Selecting relevant subsample
      
      recovered_list <- fn_identifying_stocks(df_window, payoffname, controls_levels, controls_logs, month_fe)
      
      output_i[j, 1:n_outputs] <- unlist(recovered_list, use.names = FALSE)
      
    }
    
    # output <- rbind(output, output_i)
    print(output_i)
    
  }
  
  output <- data.frame(results)
  colnames(output) <- c("year_start", "month_start", "year_end", "month_end", 
                        "permno", "n_obs", "n_factors", "R2_long", "R2_short",
                        "beta_0", "beta_1", "zeta_0", "var_eps", 
                        "out_long", "out_short", "tau_pi", "tau_pi_R")
  
  return(output)
  
}
