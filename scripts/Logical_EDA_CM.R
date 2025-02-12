# Logical EDA 

# Last Updated Tuesday February 11th, 2025 5:40PM

library(dplyr)
library(readr)

# add nrow argument 
# read_csv readr (tidy) 
df <- read_csv("../../tea/bernado/TEA_2019.csv", nmax = 100)


#save png file
# comit and push that 


#colSums(is.na(df))

data.frame(colSums(is.na(df))) %>% 
  mutate(total_na = colSums.df.) %>% 
  select(total_na) %>% 
  arrange(desc(total_na))