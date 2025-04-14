outlier_bounds = function(df){
  # calculate outlier bounds for each variable in df
  bounds = df %>% 
    summarise(across(everything(), list(
      lower = ~ quantile(., 0.25) - 1.5 * IQR(.),
      upper = ~ quantile(., 0.75) + 1.5 * IQR(.)
    )))
  
  return(bounds)
}