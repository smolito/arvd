library(tidyverse)

data_vino = read.csv("data-input/ukol_A_vino.csv") %>% 
  select(-c(X))

nas = data_vino %>% 
  filter(if_any(everything(), is.na))

if (nrow(nas) == 0){
  message("no rows contain NA values")
}

colnames(data_vino)

# all attributes numerical, no missing values
summary(data_vino)

data_vino_norm = imap_dfr(data_vino, \(x, idx){
  if(idx != "quality"){
    x = (x - min(x)) / (max(x) - min(x))
  }
  return(x)
})

# TODO: pokračovat vizualizacemi závislostí proměnných na kvalitě představuju si pomocí pairs

# acidities seem to trend w density

data_vino %>%
  select(fixed.acidity, volatile.acidity, citric.acid, density) %>% 
  pairs()

# acidites seem to not trend w pH

data_vino %>%
  select(fixed.acidity, volatile.acidity, citric.acid, pH) %>% 
  pairs()

data_vino %>%
  select(density, pH, alcohol, sweet, quality) %>% 
  pairs()
