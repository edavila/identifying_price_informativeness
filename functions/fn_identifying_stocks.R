
fn_identifying_stocks <- function(df_input, payoffname, controls_levels, controls_logs, month_fe) {
    
  df <- df_input
  
  if (length(controls_logs) != 0 ) {
  controls_logs_lagged <- vector(length = length(controls_logs))
  for (i in 1:length(controls_logs)) {
    varname <- paste("delta_log_", controls_logs[i], sep = "")
    controls_logs_lagged[i] = varname
    df <- df %>% mutate(!!varname := log(get(controls_logs[i])) - log(dplyr::lag(get(controls_logs[i]), n = 1, default = NA)))
  }
  }
  
  if (length(controls_levels) != 0 ) {
  controls_levels_lagged <- vector(length = length(controls_levels))
  for (i in 1:length(controls_levels)) {
    varname <- paste("delta_", controls_levels[i], sep = "")
    controls_levels_lagged[i] = varname
    df <- df %>% mutate(!!varname := get(controls_levels[i]) - dplyr::lag(get(controls_levels[i]), n = 1, default = NA))
  }
  }
  
  if (length(controls_levels) != 0 | length(controls_levels) != 0) {
  controls_lagged = append(controls_logs_lagged, controls_levels_lagged)
  df <- df %>% filter_at(controls_lagged, any_vars(!is.na(.)))
  } else {
    controls_lagged = c()
  }
  
  dep_var <- "delta_log_price"
  
  if (payoffname == "logistic") {
    indep_var_long  <- c("delta_log_payoff_logistic", "delta_log_payoff_logistic_future")
    indep_var_short <- c("delta_log_payoff_logistic")
  }
  
  if (payoffname == "growth") {
    indep_var_long  <- c("delta_log_payoff_growth", "delta_log_payoff_growth_future")
    indep_var_short <- c("delta_log_payoff_growth")
  }  
  
  indep_var_long  <- append(indep_var_long,  controls_lagged)
  indep_var_short <- append(indep_var_short, controls_lagged)
  
  if (month_fe == TRUE) {
    indep_var_long  <- append(indep_var_long,  "factor(month)")
    indep_var_short <- append(indep_var_short, "factor(month)")
  }
  
  f_long  <- as.formula(paste(dep_var, paste(indep_var_long,  collapse = " + "), sep = " ~ "))
  f_short <- as.formula(paste(dep_var, paste(indep_var_short, collapse = " + "), sep = " ~ "))
  
  # Running regressions
  
  reg_long  <- lm(f_long,  data = df)
  reg_short <- lm(f_short, data = df)
  
  # Outputs
  
  lev_long  <- hat(model.matrix(reg_long))  # Leverage of each observation
  lev_short <- hat(model.matrix(reg_short)) # Leverage of each observation
  
  R2_long       <- summary(reg_long)$r.squared
  R2_short      <- summary(reg_short)$r.squared
  beta_0        <- summary(reg_long)$coefficients[2] # should be [1] without constant
  beta_1        <- summary(reg_long)$coefficients[3] # should be [2] without constant
  zeta_0        <- summary(reg_short)$coefficients[2]
  var_eps       <- summary(reg_long)$sigma^2
  outlier_long  <- max(lev_long)  # Greatest outlier
  outlier_short <- max(lev_short) # Greatest outlier
  tau_pi        <- beta_1^2/var_eps
  tau_pi_R      <- (R2_long - R2_short)/(1 - R2_short)
  
  year_start  <- df_input$year[1]
  month_start <- df_input$month[1]
  year_end    <- df_input$year[length(df_input$year)]
  month_end   <- df_input$month[length(df_input$month)]
  
  permno <- df$permno[1]
  n_obs  <- df %>% nrow() %>% as.numeric()
  
  n_factors <- length(reg_long$coefficients) - length(reg_long$model) + 2
  
  output <- list(year_start, month_start, year_end, month_end, 
                 permno, n_obs, n_factors,
                 R2_long, R2_short, beta_0, beta_1, zeta_0, var_eps,
                 outlier_long, outlier_short, tau_pi, tau_pi_R)
  
  return(output)
}
