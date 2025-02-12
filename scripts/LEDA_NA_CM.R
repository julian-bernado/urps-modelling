# Logical EDA 

# Last Updated Tuesday February 11th, 2025 10:13PM

library(dplyr)
library(readr)
library(tidyverse)

# add nrow argument 
# read_csv readr (tidy) 
df <- read_csv("../../tea/bernado/TEA_2019.csv", n_max = 100)


#save png file
# comit and push that 


#colSums(is.na(df))

na_df = data.frame(colSums(is.na(df))) %>% 
  mutate(total_na = colSums.is.na.df..) %>% 
  select(total_na) 

na_df %>% 
  arrange(desc(total_na))

na_df %>% 
  dplyr::filter(total_na == 0)

na_df %>% 
  ggplot() +
  geom_histogram(aes(x = total_na))
ggsave("na.pdf")

# Need to review regex
#na_df %>% group_by('(.*)(?=_$)')