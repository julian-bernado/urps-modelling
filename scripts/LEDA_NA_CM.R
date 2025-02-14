# Logical EDA 

library(dplyr)
library(readr)
library(tidyverse)

# temporary n_max so it runs faster, will change for final run  
df <- read_csv("../../tea/bernado/TEA_2019.csv", n_max = 100)


na_df = data.frame(colSums(is.na(df))) %>% 
  mutate(total_na = colSums.is.na.df..) %>% 
  select(total_na) 

na_df %>% 
  arrange(desc(total_na))

na_df %>% 
  dplyr::filter(total_na == 0)

na_df %>% 
  dplyr::filter(total_na > nrow(df) * 0.75)

na_df %>% 
  dplyr::filter(total_na > nrow(df) * 0.90)

na_df %>% 
  dplyr::filter(total_na == nrow(df))

na_df %>% 
  ggplot() +
  geom_histogram(aes(x = total_na))
ggsave("na.pdf")

df %>%
  rowwise() %>%
  mutate(has_na = any(is.na(across(starts_with("homeless_"))))) %>%
  ungroup() %>% 
  select(starts_with("homeless_"), has_na)

df %>%
  rowwise() %>%
  mutate(all_na = all(is.na(across(starts_with("homeless_"))))) %>%
  ungroup() %>% 
  select(starts_with("homeless_"), all_na)


df %>%
  rowwise() %>%
  mutate(num_times_homeless = sum(starts_with("homeless_"))) %>%
  ungroup() 