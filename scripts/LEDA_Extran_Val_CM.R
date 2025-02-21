# Searching for Extraneous Values 

library(dplyr)
library(readr)
library(tidyverse)

# add nrow argument 
# read_csv readr (tidy) 
df <- read_csv("../../tea/bernado/TEA_2019.csv")

# replace ID 

str(df)

# Searching for extraneous values 
# 5-number summary 
df %>% 
  select_if(is.numeric) %>% 
  sapply(MARGIN = 2, summary) 

# Structure of the Data

# Counting Unique School and district 
# School USE ENROLL INSTEAD it'll have the year id but just see what district they were in enrolled in 
# df %>% select(schoolid_nces_assess_p0) %>% unique()
# 
# #District
# df %>% select(districtid_nces_assess_p0) %>% unique(districtid_nces_assess_p0)
# 
# # District with most schools 
# df %>% 
#   group_by(districtid_nces_assess_p0) %>% 
#   summarize(n_schools = unique(schoolid_nces_assess_p0)) %>% 
#   arrange(desc(n_schools)) %>% 
#   head(6)
# 
# # District with the most students 
# df %>% 
#   arrange(desc(districtid_nces_enroll_p0)) %>% 
#   head(6)



diff_scores = df %>% 
  dyplr::select(glmath_scr_m1, glmath_scr_p0, readng_scr_m1, readng_scr_p0) %>% 
  mutate(diff_math_scr = glmath_scr_p0 - glmath_scr_m1, 
         diff_read_scr = readng_scr_p0 - readng_scr_m1)

# when can visualize these histograms will see if there are any clear outliers
diff_scores %>% 
  ggplot(aes(diff_math_scr)) +
  geom_histogram()

ggsave("math_diff.png")
  
diff_scores %>% 
  ggplot(aes(diff_read_scr)) +
  geom_histogram()

ggsave("read_diff.png")

# free-reduced lunch, how many years 
df %>%
  dpylr::select(starts_with("frl_")) %>% 
  rowwise() %>% 
  mutate(any_frl = any(across(starts_with("frl_")))) %>%
  ungroup() %>% 
  print()

df %>%
  dyplr::select(starts_with("lep_")) %>% 
  rowwise() %>% 
  mutate(any_frl = any(across(starts_with("lep_")))) %>%
  ungroup() %>% 
  print()