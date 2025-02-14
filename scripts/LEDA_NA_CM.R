# Logical EDA 

library(dplyr)
library(readr)
library(tidyverse)

df <- read_csv("../../tea/bernado/TEA_2019.csv")

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

# Finding percentage of rows that have any NA 
1 - nrow(df %>% drop_na()) / nrow(df)

# Finding rows with no missing score data
no_missing_test = df %>% 
  select(glmath_scr_m1, glmath_scr_m2, glmath_scr_p0, glmath_scr_p1, glmath_scr_p2, 
         readng_scr_m1, readng_scr_m2, readng_scr_p0, readng_scr_p1, readng_scr_p2) %>% 
  drop_na()

nrow(no_missing_test) / nrow(df)

df %>%
  rowwise() %>%
  mutate(has_na = any(is.na(across(starts_with("homeless_"))))) %>%
  ungroup() %>% 
  select(starts_with("homeless_"), has_na) %>% 
  print()

df %>%
  rowwise() %>%
  mutate(all_na = all(is.na(across(starts_with("homeless_"))))) %>%
  ungroup() %>% 
  select(starts_with("homeless_"), all_na) %>% 
  print()


df %>%
  rowwise() %>%
  mutate(num_times_homeless = sum(starts_with("homeless_"))) %>%
  ungroup() %>% 
  print()