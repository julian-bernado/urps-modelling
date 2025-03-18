# Package Download 
library(dplyr)
library(readr)
library(tidyverse)
library(lme4)
library(stats)

# Data set Download 
df <- read_csv("TEA_2019.csv")

# Preprocessing 
cols_all_na = data.frame(colSums(is.na(df))) %>% 
  mutate(total_na = colSums.is.na.df..) %>% 
  select(total_na) %>% 
  filter(total_na == nrow(df)) %>% 
  row.names()

third_grade = df %>% 
  filter(gradelevel == 3, !is.na(glmath_scr_p0)) 

na_third_grade = data.frame(colSums(is.na(third_grade))) %>% 
  mutate(total_na = colSums.is.na.third_grade..) %>% 
  select(total_na) %>% 
  filter(total_na >= .95* nrow(third_grade)) %>% 
  row.names()

no_variance = c('acadyear', 'gradelevel', 'dropout_inferred_m1', 
                'persist_inferred_m1', 'glmath_ver_p0', 'readng_ver_p0')

third_grade = third_grade %>% 
  select(!all_of(na_third_grade))

third_grade = third_grade %>% 
  select(!all_of(no_variance))

fourth_grade = df %>% 
  filter(gradelevel == 4, !is.na(glmath_scr_p0)) 

na_fourth_grade = data.frame(colSums(is.na(fourth_grade))) %>% 
  mutate(total_na = colSums.is.na.fourth_grade..) %>% 
  select(total_na) %>% 
  filter(total_na >= .95* nrow(fourth_grade)) %>% 
  row.names()

fourth_grade = fourth_grade %>% 
  select(!all_of(na_fourth_grade))

fifth_grade = df %>% 
  filter(gradelevel == 5, !is.na(glmath_scr_p0)) 

na_fifth_grade = data.frame(colSums(is.na(fifth_grade))) %>% 
  mutate(total_na = colSums.is.na.fifth_grade..) %>% 
  select(total_na) %>% 
  filter(total_na >= .95* nrow(fifth_grade)) %>% 
  row.names()

fifth_grade = fifth_grade %>% 
  select(!all_of(na_fifth_grade))

# Selecting 30% of Schools (for easier plot viewing)

school_ids = third_grade %>% 
  select(schoolid_nces_enroll_p0) %>% 
  unique() %>% 
  pull()

selected_schools = sample(x = school_ids, size = length(school_ids) * 0.3, replace = FALSE)

limited_third_grade = third_grade %>% 
  filter(schoolid_nces_enroll_p0 %in% selected_schools)

limited_fourth_grade = fourth_grade %>% 
  filter(schoolid_nces_enroll_p0 %in% selected_schools)

limited_fifth_grade = fifth_grade %>% 
  filter(schoolid_nces_enroll_p0 %in% selected_schools)

# Math versus other variable plots 3rd Grade 
fourth_grade %>% colnames()

limited_fourth_grade %>% 
  ggplot(aes(x = readng_scr_m1, y = glmath_scr_p0)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

limited_fourth_grade %>% 
  ggplot(aes(x = readng_scr_m1, y = glmath_scr_p0)) + 
  geom_point() + 
  geom_smooth()

limited_fifth_grade %>% 
  ggplot(aes(x = readng_scr_m1, y = glmath_scr_p0)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

limited_fifth_grade %>% 
  ggplot(aes(x = readng_scr_m1, y = glmath_scr_p0)) + 
  geom_point() + 
  geom_smooth()

# Based on Gender Not a Huge Difference Based on Gender 

third_grade %>% 
  ggplot(aes(x = as.factor(gender), y = glmath_scr_p0)) + 
  geom_boxplot()

third_grade %>% 
  group_by(gender) %>% 
  filter(!is.na(glmath_scr_p0)) %>% 
  summarize(mean_math_scr = mean(glmath_scr_p0), 
            stan_dev_math_scr = sd(glmath_scr_p0))

fourth_grade %>% 
  ggplot(aes(x = as.factor(gender), y = glmath_scr_p0)) + 
  geom_boxplot()

fourth_grade %>% 
  group_by(gender) %>% 
  filter(!is.na(glmath_scr_p0)) %>% 
  summarize(mean_math_scr = mean(glmath_scr_p0), 
            stan_dev_math_scr = sd(glmath_scr_p0))

fifth_grade %>% 
  ggplot(aes(x = as.factor(gender), y = glmath_scr_p0)) + 
  geom_boxplot()

fifth_grade %>% 
  group_by(gender) %>% 
  filter(!is.na(glmath_scr_p0)) %>% 
  summarize(mean_math_scr = mean(glmath_scr_p0), 
            stan_dev_math_scr = sd(glmath_scr_p0))

# Ethnicity -- group 2 consistently scoring higher 
# group 2 is Asian 

third_grade %>% 
  ggplot(aes(x = as.factor(raceth), y = glmath_scr_p0)) + 
  geom_boxplot()

third_grade %>% 
  group_by(raceth) %>% 
  filter(!is.na(glmath_scr_p0)) %>% 
  summarize(mean_math_scr = mean(glmath_scr_p0), 
            stan_dev_math_scr = sd(glmath_scr_p0))

fourth_grade %>% 
  ggplot(aes(x = as.factor(raceth), y = glmath_scr_p0)) + 
  geom_boxplot()

fourth_grade %>% 
  group_by(raceth) %>% 
  filter(!is.na(glmath_scr_p0)) %>% 
  summarize(mean_math_scr = mean(glmath_scr_p0), 
            stan_dev_math_scr = sd(glmath_scr_p0))

fifth_grade %>% 
  ggplot(aes(x = as.factor(gender), y = glmath_scr_p0)) + 
  geom_boxplot()

fifth_grade %>% 
  group_by(raceth) %>% 
  filter(!is.na(glmath_scr_p0)) %>% 
  summarize(mean_math_scr = mean(glmath_scr_p0), 
            stan_dev_math_scr = sd(glmath_scr_p0))

## they around around 4% of the population
(third_grade %>% filter(raceth == 2) %>% nrow() / third_grade %>% nrow())
(fourth_grade %>% filter(raceth == 2) %>% nrow() / third_grade %>% nrow())
(fifth_grade %>% filter(raceth == 2) %>% nrow() / third_grade %>% nrow())

# FRL 