# Searching for Extraneous Values 

library(dplyr)
library(readr)
library(tidyverse)

# add nrow argument 
# read_csv readr (tidy) 
df <- read_csv("../../tea/bernado/TEA_2019.csv", n_max = 100)

# Counting Unique School and district 
# School
df %>% 
  select(schoolid_nces_assess_p0) %>% 
  unique()

#District
df %>% 
  select(schoolid_nces_assess_p0) %>% 
  unique(districtid_nces_assess_p0)

# District with most schools 
df %>% 
  group_by(schoolid_nces_assess_p0) %>% 
  summarize(n_schools = n(districtid_nces_assess_p0))



# 5-number summary 
df %>% 
  select_if(is.numeric) %>% 
  sapply(MARGIN = 2, summary) 

diff_scores = df %>% 
  select(glmath_scr_m1, glmath_scr_p0, reading_scr_m1, reading_scr_p0) %>% 
  mutate(diff_math_scr = glmath_scr_p0 - glmath_scr_m1, 
         diff_read_scr = reading_scr_p0 - reading_scr_m1)

# when can visualize these histograms will see if there are any clear outliers
diff_scores %>% 
  ggplot(aes(diff_math_scr)) +
  geom_histogram()
  
diff_scores %>% 
  ggplot(aes(diff_read_scr)) +
  geom_histogram()

# free-reduced lunch, how many years 
df %>%
  select(starts_with("frl_")) %>% 
  rowwise() %>% 
  mutate(any_frl = any(across(starts_with("frl_")))) %>%
  ungroup() 

df %>%
  select(starts_with("lep_")) %>% 
  rowwise() %>% 
  mutate(any_frl = any(across(starts_with("lep_")))) %>%
  ungroup() 