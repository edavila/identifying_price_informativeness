library(slider)

ivol <- df_q %>% 
  mutate(aret = ret - vwretd) %>% 
  select(permno, year, month, year, aret) %>% 
  group_by(permno) %>% 
  mutate(ivol = slide_dbl(aret, sd, .before = 29, .after = 0, .complete = TRUE)) %>% 
  select(-aret)
  