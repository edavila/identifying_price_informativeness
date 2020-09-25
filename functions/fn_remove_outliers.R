
remove_outliers <- function(input_df, out_threshold = 0.5){
  
  output <- input_df %>% 
    filter(out_short < out_threshold & 
            out_long < out_threshold)
  
  return(output)
  
}