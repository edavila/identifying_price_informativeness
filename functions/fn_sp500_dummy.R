
fn_sp500_dummy <- function(crsp, sp500){
  
  sp500 <- sp500 %>% group_by(permno) %>% mutate(id = row_number())
  
  setDT(sp500)
  sp500wide <- dcast(sp500, permno ~ id, value.var = c("start", "ending"))
  crsp <- left_join(crsp, sp500wide, by = "permno")
  crsp <- mutate(crsp, sp500 = 0)
  
  crsp <- crsp %>% mutate(sp500 = replace(sp500, date >= start_1 & date <= ending_1 & !is.na(start_1), 1))
  crsp <- crsp %>% mutate(sp500 = replace(sp500, date >= start_2 & date <= ending_2 & !is.na(start_2), 1))
  crsp <- crsp %>% mutate(sp500 = replace(sp500, date >= start_3 & date <= ending_3 & !is.na(start_3), 1))
  crsp <- crsp %>% mutate(sp500 = replace(sp500, date >= start_4 & date <= ending_4 & !is.na(start_4), 1))
  
  output <- crsp %>% select(-starts_with("start_")) %>% select(-starts_with("ending_"))
  
  return(output)
}