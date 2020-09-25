
library(moments)

# input_df <-  results_a; namefile <- "results_a"

sum_results <- function(input_df, namefile){
  
  results <- input_df %>%
    select(tau_pi:tau_pi_R) %>% 
    summarize(across(everything(), 
                     list(mean   = ~ mean(.,          na.rm = T),
                          sd     = ~ sd(.,            na.rm = T),
                          skew   = ~ skewness(.,      na.rm = T),
                          p10    = ~ quantile(., 0.1, na.rm = T, names = F),
                          median = ~ median(.,        na.rm = T),
                          p90    = ~ quantile(., 0.9, na.rm = T, names = F)),
              .names = "{fn}.{col}")) %>% 
    pivot_longer(everything()) %>% 
    separate(name, c("stat", "var"), sep = "(\\.)") %>% 
    pivot_wider(names_from = stat, values_from = value) %>% 
    mutate(across(where(is.numeric), format, digits = 4, nsmall = 2)) %>% 
    rename("Mean"     = mean, 
           "St. Dev"  = sd, 
           "Skewness" = skew, 
           "P10"      = p10, 
           "Median"   = median, 
           "P90"      = p90) %>% 
    data.frame() %>%  
    select(-var)
  
  rownames(results) <- c("Absolute Price Informativeness", "Relative Price Informativeness")

  directory <- "output/output_tables/"
  
  kab      <- kable(results, "latex", booktabs = T)
  path     <- paste(directory, namefile, ".tex", sep = "")
  fileConn <- file(path); writeLines(kab, fileConn); close(fileConn)
  
  p <- input_df %>% 
    ggplot() + 
    aes(tau_pi_R) + 
    geom_histogram(bins = 30) + 
    labs(x = "Relative Price Informativeness", 
         y = "Number of Securities") + 
    theme_classic()
  
  path <- paste(directory, namefile, ".pdf", sep = "")
  
  ggsave(path, plot = p, width = 7, height = 5, units = "in", dpi = 300)
  
  # p8 <- ggplot(input_df, aes(tau_pi_R)) + 
  #   geom_density(alpha = 0.3) + 
  #   xlim(0, 0.4) + ylim(0, 10) + 
  #   labs(x = "Relative Price Informativeness")
  # 
  return()
}

# input_df <-  df_q; results  <- results_q; n <- 20
  
build_charac <- function(input_df, df_io_share_permno, results, n = 20){
  
  df_charac <- input_df %>%
    mutate(booktomarket = ifelse(booktomarket < 0, NA, booktomarket),
           size         = log(mcap),
           aret         = ret - vwretd) %>% 
    group_by(permno) %>% 
    summarize(across(c(size, turnover, sp500, booktomarket), list(mean = ~ mean(., na.rm = T)), .names = "{fn}_{col}"),
              across(c(hexcd, sic1, sic2), list(last = last), .names = "{fn}_{col}"),
              across(aret, sd), .groups = "drop_last") %>% 
    rename(mean_ivol = aret) %>% 
    mutate(sp500f = factor(mean_sp500 >= 0.5),
           sp500f = fct_recode(sp500f, "sp500" = "TRUE", "NO sp500" = "FALSE")) %>%
    rename(share_sp500 = mean_sp500) %>% 
    left_join(df_io_share_permno, by = "permno") %>%
    mutate(mean_io = avg_share) %>% 
    right_join(results,           by = "permno") %>% 
    mutate(across(starts_with("mean_"), list(ntile = ~ntile(.,n)), .names = "{fn}_{col}")) %>% 
    group_by(ntile_mean_size)         %>% mutate(ntile_size     = mean(mean_size, na.rm = T),         tau_pi_R_size     = mean(tau_pi_R, na.rm = T)) %>% ungroup() %>%
    group_by(ntile_mean_turnover)     %>% mutate(ntile_turnover = mean(mean_turnover, na.rm = T),     tau_pi_R_turnover = mean(tau_pi_R, na.rm = T)) %>% ungroup() %>% 
    group_by(ntile_mean_booktomarket) %>% mutate(ntile_book     = mean(mean_booktomarket, na.rm = T), tau_pi_R_book     = mean(tau_pi_R, na.rm = T)) %>% ungroup() %>% 
    group_by(ntile_mean_ivol)         %>% mutate(ntile_ivol     = mean(mean_ivol, na.rm = T),         tau_pi_R_ivol     = mean(tau_pi_R, na.rm = T)) %>% ungroup() %>% 
    group_by(ntile_mean_io)           %>% mutate(ntile_io       = mean(mean_io, na.rm = T),           tau_pi_R_io       = mean(tau_pi_R, na.rm = T)) %>% ungroup()
  
  return(df_charac)
}

# input_df <- results_charac_q; namefile <- "results_charac_q"
  
fig_all <- function(input_df, namefile){
  
  p1 <- ggplot(input_df, aes(factor(last_sic1),  tau_pi_R)) + 
    geom_boxplot(color = "red", fill = "orange", alpha = 0.2) + 
    labs(x = "Industry (One-digit SIC)", y = "Relative Price Informativeness") + 
    theme_classic()
  
  p2 <- ggplot(input_df, aes(factor(last_sic2),  tau_pi_R)) + 
    geom_boxplot() + labs(x = "2 digit SIC", y = "Relative Price Informativeness") + 
    theme_classic()
  
  p3a <- ggplot(input_df, aes(factor(last_hexcd), tau_pi_R)) + 
    geom_boxplot(color = "red", fill = "orange", alpha = 0.2) + 
    labs(x = "Exchange", y = "Relative Price Informativeness") + 
    theme_classic()
  
  p3b <- ggplot(input_df, aes(tau_pi_R, fill = last_hexcd, color = last_hexcd)) + 
    geom_density(alpha = 0.3) + 
    xlim(0,0.4) +
    labs(x = "Relative Price Informativeness") +
    guides(fill = guide_legend(title = "Exchange")) + 
    scale_colour_discrete(guide = FALSE) + 
    theme_classic()
  
  p4 <- ggplot(input_df, aes(mean_size, tau_pi_R)) + 
    geom_point(color = "black", fill = "orange", alpha = 0.9) +
    geom_smooth(method = lm) + # scale_x_continuous(trans = "log") +
    labs(x = "Market Capitalization (log)", y = "Relative Price Informativeness") +
    theme_classic()
  
  p5 <- ggplot(input_df, aes(mean_turnover, tau_pi_R)) +
    geom_point(color = "black", fill = "orange", alpha = 0.9) +
    geom_smooth(method = lm) + 
    labs(x = "Turnover", y = "Relative Price Informativeness")
  
  p6 <- ggplot(input_df, aes(factor(sp500f), tau_pi_R)) +
    geom_boxplot(color = "red", fill = "orange", alpha = 0.2) +
    labs(x = "SP500", y = "Relative Price Informativeness") +
    theme_classic()
  
  p7 <- ggplot(input_df, aes(share_sp500, tau_pi_R)) + 
    geom_point(color = "black", fill = "orange", alpha = 0.9) + 
    geom_smooth(method = lm) + 
    labs(x = "SP500", y = "Relative Price Informativeness")
  
  p9 <- ggplot(input_df, aes(mean_booktomarket, tau_pi_R)) +
    geom_point(color = "black", fill = "orange", alpha = 0.9) +
    geom_smooth(method = lm) + 
    labs(x = "Book/Market", y = "Relative Price Informativeness")
  
  directory <- "output/output_figures"
  
  # ggsave(paste(directory, "/", namefile, "sector_1",         ".pdf", sep = ""), plot = p1,  width = 10, height = 6, units = "in", dpi = 300)
  # ggsave(paste(directory, "/", namefile, "sector_2",         ".pdf", sep = ""), plot = p2,  width = 8,  height = 6, units = "in", dpi = 300)
  # ggsave(paste(directory, "/", namefile, "exchange_box",     ".pdf", sep = ""), plot = p3a, width = 8,  height = 6, units = "in", dpi = 300)
  # ggsave(paste(directory, "/", namefile, "exchange_density", ".pdf", sep = ""), plot = p3b, width = 8,  height = 6, units = "in", dpi = 300)
  # ggsave(paste(directory, "/", namefile, "mcap",             ".pdf", sep = ""), plot = p4,  width = 8,  height = 6, units = "in", dpi = 300)
  # ggsave(paste(directory, "/", namefile, "turnover",         ".pdf", sep = ""), plot = p5,  width = 8,  height = 6, units = "in", dpi = 300)
  # ggsave(paste(directory, "/", namefile, "sp500",            ".pdf", sep = ""), plot = p6,  width = 8,  height = 6, units = "in", dpi = 300)
  # ggsave(paste(directory, "/", namefile, "sp500c",           ".pdf", sep = ""), plot = p7,  width = 8,  height = 6, units = "in", dpi = 300)
  # ggsave(paste(directory, "/", namefile, "exchange",         ".pdf", sep = ""), plot = p8,  width = 8,  height = 6, units = "in", dpi = 300)
  # ggsave(paste(directory, "/", namefile, "booktomarket",     ".pdf", sep = ""), plot = p9,  width = 8,  height = 6, units = "in", dpi = 300)
  
  p <- plot_grid(p1, p2, p3a, p3b, p4, p5, p6, p7, p9, ncol = 3, nrow = 3)
  ggsave(paste(directory, "/", namefile, "all_characteristics_", ".pdf", sep = ""), plot = last_plot(), width = 16, height = 12, units = "in", dpi = 300)
  
  return()
}

fig_binned <- function(input_df, namefile){
  
  df_size     <- input_df %>% group_by(ntile_mean_size)         %>% summarize(across(c(ntile_size,     tau_pi_R_size),     last))
  df_turnover <- input_df %>% group_by(ntile_mean_turnover)     %>% summarize(across(c(ntile_turnover, tau_pi_R_turnover), last))
  df_book     <- input_df %>% group_by(ntile_mean_booktomarket) %>% summarize(across(c(ntile_book,     tau_pi_R_book),     last))
  df_ivol     <- input_df %>% group_by(ntile_mean_ivol)         %>% summarize(across(c(ntile_ivol,     tau_pi_R_ivol),     last))
  df_io       <- input_df %>% group_by(ntile_mean_io)           %>% summarize(across(c(ntile_io,       tau_pi_R_io),       last))
  
  p1 <- ggplot(df_size, aes(ntile_size, tau_pi_R_size)) + 
    geom_point(color = "black", fill = "orange", alpha = 0.9) + 
    geom_smooth(method = lm, se = F) + 
    labs(x = "Size", 
         y = "Relative Price Informativeness") + 
    theme_classic() +
    theme(text = element_text(size = 24))
  
  p2 <- ggplot(df_turnover, aes(ntile_turnover, tau_pi_R_turnover)) + 
    geom_point(color = "black", fill = "orange", alpha = 0.9) + 
    geom_smooth(method = lm, se = F) + 
    labs(x = "Turnover", 
         y = "Relative Price Informativeness") + 
    theme_classic() +
    theme(text = element_text(size = 24))
  
  p3 <- ggplot(df_book, aes(ntile_book, tau_pi_R_book)) + 
    geom_point(color = "black", fill = "orange", alpha = 0.9) + 
    geom_smooth(method = lm, se = F) + 
    labs(x = "Book/Market", 
         y = "Relative Price Informativeness") + 
    theme_classic() +
    theme(text = element_text(size = 24))
  
  p4 <- ggplot(df_ivol, aes(ntile_ivol, tau_pi_R_ivol)) + 
    geom_point(color = "black", fill = "orange", alpha = 0.9) + 
    geom_smooth(method = lm, se = F) + 
    labs(x = "Idiosyncratic Return Volatility", 
         y = "Relative Price Informativeness") + 
    theme_classic() +
    theme(text = element_text(size = 24))
  
  p5 <- ggplot(df_io, aes(ntile_io, tau_pi_R_io)) + 
    geom_point(color = "black", fill = "orange", alpha = 0.9) + 
    geom_smooth(method = lm, se = F) + 
    labs(x = "Institutional Ownership", 
         y = "Relative Price Informativeness") + 
    theme_classic() +
    theme(text = element_text(size = 24))
  
  directory <- "output/output_figures"
  
  ggsave(paste(directory, "/", namefile, "size.pdf",     sep = ""), plot = p1, width = 6, height = 6, units = "in", dpi = 300)
  ggsave(paste(directory, "/", namefile, "turnover.pdf", sep = ""), plot = p2, width = 6, height = 6, units = "in", dpi = 300)
  ggsave(paste(directory, "/", namefile, "value.pdf",    sep = ""), plot = p3, width = 6, height = 6, units = "in", dpi = 300)
  ggsave(paste(directory, "/", namefile, "ivol.pdf",     sep = ""), plot = p4, width = 6, height = 6, units = "in", dpi = 300)
  ggsave(paste(directory, "/", namefile, "inst_own.pdf", sep = ""), plot = p5, width = 6, height = 6, units = "in", dpi = 300)
  
  p <- plot_grid(p1, p2, p3, p4, p5, ncol = 3, nrow = 2)
  ggsave(paste(directory, "/", namefile, "all", ".pdf", sep = ""), plot = last_plot(), width = 16, height = 12, units = "in", dpi = 300)
  
  return()
}
