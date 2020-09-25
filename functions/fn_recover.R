
fn_recover <- function(df_input, payoffname, controls_levels, controls_logs, month_fe){
  
  permno_list <- df_input %>% group_by(permno) %>% count()
  n_permno    <- permno_list %>% nrow() %>% as.numeric()
  
  n_outputs <- 17
  
  output <- matrix(ncol = n_outputs, nrow = n_permno) # Empty data frame to store results
  
  for (i in 1:n_permno) {
    
    per       <- permno_list$permno[i]
    df_permno <- df_input %>% filter(permno == per)
    
    recovered_list <- fn_identifying_stocks(df_permno, payoffname, controls_levels, controls_logs, month_fe)
    
    output[i, 1:n_outputs] <- unlist(recovered_list, use.names = FALSE)
    print(per)
    }
  
  output <- data.frame(output)
  colnames(output) <- c("year_start", "month_start", "year_end", "month_end", 
                        "permno", "n_obs", "R2_long", "R2_short",
                        "beta_0", "beta_1", "zeta_0", "var_eps", 
                        "out_long", "out_short", "tau_pi", "tau_pi_R")
  
  return(output)
  
}

fn_recover_par <- function(df_input, payoffname, controls_levels, controls_logs, month_fe){
  
  permno_list <- df_input %>% group_by(permno) %>% count()
  n_permno    <- permno_list %>% nrow() %>% as.numeric()
  
  results <- foreach(i = 1:n_permno, .combine = "rbind", .export = c('fn_identifying_stocks'), .packages = c("tidyverse", "AER")) %dopar% {
    
    per       <- permno_list$permno[i]
    df_permno <- df_input %>% filter(permno == per)
    out       <- fn_identifying_stocks(df_permno, payoffname, controls_levels, controls_logs, month_fe)
    
    print(unlist(out))
    #print(i)
  }
  
  output <- data.frame(results) # as_tibble
  
  colnames(output) <- c("year_start", "month_start", "year_end", "month_end", 
                    "permno", "n_obs", "n_factors", "R2_long", "R2_short",
                    "beta_0", "beta_1", "zeta_0", "var_eps", 
                    "out_long", "out_short", "tau_pi", "tau_pi_R")

  
  return(output)
  
}