library(tidyverse)

data_vino = read.csv("data-input/ukol_A_vino.csv") %>% 
  select(-c(X))

nas = data_vino %>% 
  filter(if_any(everything(), is.na))

if (nrow(nas) == 0){
  message("no rows contain NA values")
}

summary(data_vino)

# GOAL IS TO FIND ATRIBUTES WITH THE MOST FOOLPROOF TRENDS

# SULFUR MŮŽE KORELOVAT S QUALITY, UMĚLE SE DODÁVÁ

# all atributes numerical, no missing values

boxplot(data_vino$fixed.acidity) # kinda sus

colnames(data_vino)

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
