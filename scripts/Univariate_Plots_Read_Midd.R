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

sixth_grade = df %>% 
  filter(gradelevel == 6, !is.na(readng_scr_p0)) 

na_sixth_grade = data.frame(colSums(is.na(sixth_grade))) %>% 
  mutate(total_na = colSums.is.na.sixth_grade..) %>% 
  select(total_na) %>% 
  filter(total_na >= .95* nrow(sixth_grade)) %>% 
  row.names()

no_variance = c('acadyear', 'gradelevel', 'dropout_inferred_m1', 
                'persist_inferred_m1', 'readng_ver_p0', 'readng_ver_p0')

sixth_grade = sixth_grade %>% 
  select(!all_of(na_sixth_grade))

sixth_grade = sixth_grade %>% 
  select(!all_of(no_variance))

seventh_grade = df %>% 
  filter(gradelevel == 7, !is.na(readng_scr_p0)) 

na_seventh_grade = data.frame(colSums(is.na(seventh_grade))) %>% 
  mutate(total_na = colSums.is.na.seventh_grade..) %>% 
  select(total_na) %>% 
  filter(total_na >= .95* nrow(seventh_grade)) %>% 
  row.names()

seventh_grade = seventh_grade %>% 
  select(!all_of(na_seventh_grade))

eighth_grade = df %>% 
  filter(gradelevel == 8, !is.na(readng_scr_p0)) 

na_eighth_grade = data.frame(colSums(is.na(eighth_grade))) %>% 
  mutate(total_na = colSums.is.na.eighth_grade..) %>% 
  select(total_na) %>% 
  filter(total_na >= .95* nrow(eighth_grade)) %>% 
  row.names()

eighth_grade = eighth_grade %>% 
  select(!all_of(na_eighth_grade))

# Selecting 30% of Schools (for easier plot viewing)

school_ids = sixth_grade %>% 
  select(schoolid_nces_enroll_p0) %>% 
  unique() %>% 
  pull()

selected_schools = sample(x = school_ids, size = length(school_ids) * 0.3, replace = FALSE)

limited_sixth_grade = sixth_grade %>% 
  filter(schoolid_nces_enroll_p0 %in% selected_schools)

limited_seventh_grade = seventh_grade %>% 
  filter(schoolid_nces_enroll_p0 %in% selected_schools)

limited_eighth_grade = eighth_grade %>% 
  filter(schoolid_nces_enroll_p0 %in% selected_schools)

# Reading Score from Year Before 

limited_seventh_grade %>% 
  ggplot(aes(x = readng_scr_m1, y = readng_scr_p0)) + 
  geom_point() + 
  geom_smooth(method = 'lm')
# VERY linear relationship 

limited_seventh_grade %>% 
  ggplot(aes(x = readng_scr_m1, y = readng_scr_p0)) + 
  geom_point() + 
  geom_smooth()

limited_eighth_grade %>% 
  ggplot(aes(x = readng_scr_m1, y = readng_scr_p0)) + 
  geom_point() + 
  geom_smooth(method = 'lm')
# more sparse on bottom 

limited_eighth_grade %>% 
  ggplot(aes(x = readng_scr_m1, y = readng_scr_p0)) + 
  geom_point() + 
  geom_smooth()
# these splines don't really work because they read too much into the low scores 

## 0.6239 correlation
seventh_grade %>% 
  filter(!is.na(readng_scr_m1), !is.na(readng_scr_p0)) %>% 
  select(readng_scr_m1, readng_scr_p0) %>% 
  cor()

## 0.531 correlation
eighth_grade %>% 
  filter(!is.na(readng_scr_m1), !is.na(readng_scr_p0)) %>% 
  select(readng_scr_m1, readng_scr_p0) %>% 
  cor()

# Math versus other variable plots 3rd Grade 
limited_seventh_grade %>% 
  ggplot(aes(x = glmath_scr_m1, y = readng_scr_p0)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

limited_seventh_grade %>% 
  ggplot(aes(x = glmath_scr_m1, y = readng_scr_p0)) + 
  geom_point() + 
  geom_smooth()
# the spline fits a bit better here 

limited_eighth_grade %>% 
  ggplot(aes(x = glmath_scr_m1, y = readng_scr_p0)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

limited_eighth_grade %>% 
  ggplot(aes(x = readng_scr_m1, y = readng_scr_p0)) + 
  geom_point() + 
  geom_smooth()
# too curvy for this 

# 0.66642...
seventh_grade %>% 
  filter(!is.na(glmath_scr_m1)) %>% 
  select(glmath_scr_m1, readng_scr_p0) %>% 
  cor()

# Correlation 0.3
eighth_grade %>% 
  filter(!is.na(glmath_scr_m1)) %>% 
  select(glmath_scr_m1, readng_scr_p0) %>% 
  cor()

# generally as we move up grade levels the affect of variables lessen

# Based on Gender Not a Huge Difference Based on Gender 

sixth_grade %>% 
  ggplot(aes(x = as.factor(gender), y = readng_scr_p0)) + 
  geom_boxplot()

sixth_grade %>% 
  group_by(gender) %>% 
  filter(!is.na(readng_scr_p0)) %>% 
  summarize(mean_math_scr = mean(readng_scr_p0), 
            stan_dev_math_scr = sd(readng_scr_p0))
# 1 Female, 2 Male 
# 1 higher than 2 by 33

seventh_grade %>% 
  ggplot(aes(x = as.factor(gender), y = readng_scr_p0)) + 
  geom_boxplot()

seventh_grade %>% 
  group_by(gender) %>% 
  filter(!is.na(readng_scr_p0)) %>% 
  summarize(mean_math_scr = mean(readng_scr_p0), 
            stan_dev_math_scr = sd(readng_scr_p0))
# 1 higher than 2 by 34

eighth_grade %>% 
  ggplot(aes(x = as.factor(gender), y = readng_scr_p0)) + 
  geom_boxplot()

eighth_grade %>% 
  group_by(gender) %>% 
  filter(!is.na(readng_scr_p0)) %>% 
  summarize(mean_math_scr = mean(readng_scr_p0), 
            stan_dev_math_scr = sd(readng_scr_p0))
# 1 higher than 2 by 8 

# Ethnicity -- group 2 consistently scoring higher 
# group 2 is Asian 

sixth_grade %>% 
  ggplot(aes(x = as.factor(raceth), y = readng_scr_p0)) + 
  geom_boxplot()

sixth_grade %>% 
  group_by(raceth) %>% 
  filter(!is.na(readng_scr_p0)) %>% 
  summarize(mean_math_scr = mean(readng_scr_p0), 
            stan_dev_math_scr = sd(readng_scr_p0))

seventh_grade %>% 
  ggplot(aes(x = as.factor(raceth), y = readng_scr_p0)) + 
  geom_boxplot()

seventh_grade %>% 
  group_by(raceth) %>% 
  filter(!is.na(readng_scr_p0)) %>% 
  summarize(mean_math_scr = mean(readng_scr_p0), 
            stan_dev_math_scr = sd(readng_scr_p0))

eighth_grade %>% 
  ggplot(aes(x = as.factor(raceth), y = readng_scr_p0)) + 
  geom_boxplot()

eighth_grade %>% 
  group_by(raceth) %>% 
  filter(!is.na(readng_scr_p0)) %>% 
  summarize(mean_math_scr = mean(readng_scr_p0), 
            stan_dev_math_scr = sd(readng_scr_p0))

## they around around 4% or 3% of the population
(sixth_grade %>% filter(raceth == 2) %>% nrow() / sixth_grade %>% nrow())
(seventh_grade %>% filter(raceth == 2) %>% nrow() / sixth_grade %>% nrow())
(eighth_grade %>% filter(raceth == 2) %>% nrow() / sixth_grade %>% nrow())

# FRL 

score_diff = function(df, var_interest, exam_type){
  df %>% 
    group_by({{var_interest}}) %>% 
    summarize(mean_math = mean({{exam_type}}), 
              stan_err_math = sd({{exam_type}}), 
              perc_pop = n()/nrow(df))
}

graph_diff = function(df, var_interest, exam_type){
  df %>% 
    ggplot(aes(x = as.factor({{var_interest}}), y = {{exam_type}})) + 
    geom_boxplot()
}

## 0 pretty much always performs better 
score_diff(sixth_grade, frl_now, readng_scr_p0)
score_diff(sixth_grade, frl_ever, readng_scr_p0) # ever and elem are the same here 
score_diff(sixth_grade, frl_2yr, readng_scr_p0)
score_diff(sixth_grade, frl_elem, readng_scr_p0)

score_diff(seventh_grade, frl_now, readng_scr_p0)
score_diff(seventh_grade, frl_ever, readng_scr_p0) # ever and elem are the same here 
score_diff(seventh_grade, frl_2yr, readng_scr_p0)
score_diff(seventh_grade, frl_elem, readng_scr_p0)

score_diff(eighth_grade, frl_now, readng_scr_p0)
score_diff(eighth_grade, frl_ever, readng_scr_p0) # ever and elem are the same here 
score_diff(eighth_grade, frl_2yr, readng_scr_p0)

# LEP

## 0 performing a little bit better (for each grade)  
score_diff(sixth_grade, lep_now, readng_scr_p0)
score_diff(sixth_grade, lep_ever, readng_scr_p0) 
score_diff(sixth_grade, lep_2yr, readng_scr_p0)
score_diff(sixth_grade, lep_elem, readng_scr_p0)

score_diff(seventh_grade, lep_now, readng_scr_p0)
score_diff(seventh_grade, lep_ever, readng_scr_p0) 
score_diff(seventh_grade, lep_2yr, readng_scr_p0)
score_diff(seventh_grade, lep_elem, readng_scr_p0)

score_diff(eighth_grade, lep_now, readng_scr_p0)
score_diff(eighth_grade, lep_ever, readng_scr_p0) 
score_diff(eighth_grade, lep_2yr, readng_scr_p0)
score_diff(eighth_grade, lep_elem, readng_scr_p0)

# MIGRANT
## 1570 - 1500 = 70 difference normally 
score_diff(sixth_grade, migrant_now, readng_scr_p0)
score_diff(sixth_grade, migrant_ever, readng_scr_p0)  
score_diff(sixth_grade, migrant_2yr, readng_scr_p0)
score_diff(sixth_grade, migrant_elem, readng_scr_p0)

# 1642 - 1573 = 69
score_diff(seventh_grade, migrant_now, readng_scr_p0)
score_diff(seventh_grade, migrant_ever, readng_scr_p0)  
score_diff(seventh_grade, migrant_2yr, readng_scr_p0)
score_diff(seventh_grade, migrant_elem, readng_scr_p0)

# 1730 - 1679 = 51 diff 
score_diff(eighth_grade, migrant_now, readng_scr_p0)
score_diff(eighth_grade, migrant_ever, readng_scr_p0) 
score_diff(eighth_grade, migrant_2yr, readng_scr_p0)
score_diff(eighth_grade, migrant_elem, readng_scr_p0)

# HOMELESS 

## 1573 - 1532 = 41 difference normally 
score_diff(sixth_grade, homeless_now, readng_scr_p0)
score_diff(sixth_grade, homeless_ever, readng_scr_p0)  
score_diff(sixth_grade, homeless_2yr, readng_scr_p0)
score_diff(sixth_grade, homeless_elem, readng_scr_p0)

# 1574 - 1523 = 51
score_diff(seventh_grade, homeless_now, readng_scr_p0)
score_diff(seventh_grade, homeless_ever, readng_scr_p0)  
score_diff(seventh_grade, homeless_2yr, readng_scr_p0)
score_diff(seventh_grade, homeless_elem, readng_scr_p0)

# 1646 - 1587 = 59 diff 
score_diff(eighth_grade, homeless_now, readng_scr_p0) # 73
score_diff(eighth_grade, homeless_ever, readng_scr_p0) # 57
score_diff(eighth_grade, homeless_2yr, readng_scr_p0) # 59

# SPECIALED 
# 0 is scoring much higher 
score_diff(sixth_grade, specialed_now, readng_scr_p0) # 140
score_diff(sixth_grade, specialed_ever, readng_scr_p0)  # 132
score_diff(sixth_grade, specialed_2yr, readng_scr_p0) # 134
score_diff(sixth_grade, specialed_elem, readng_scr_p0) # 131

score_diff(seventh_grade, specialed_now, readng_scr_p0) # 148
score_diff(seventh_grade, specialed_ever, readng_scr_p0) # 142
score_diff(seventh_grade, specialed_2yr, readng_scr_p0) # 144

score_diff(eighth_grade, specialed_now, readng_scr_p0) # 128
score_diff(eighth_grade, specialed_ever, readng_scr_p0) # 121
score_diff(eighth_grade, specialed_2yr, readng_scr_p0) # 121

# nightly stopping point 
# Attendance -- have pretty low correlation and the plot doesn't show much 
# all correlations 0.07 to 0.03
limited_sixth_grade %>% 
  filter(attend_m1 < 1.01) %>% 
  ggplot(aes(x = attend_m1, y = readng_scr_p0)) + 
  geom_point() + geom_smooth()

limited_sixth_grade %>% 
  filter(attend_p0 < 1.01) %>% 
  ggplot(aes(x = attend_p0, y = readng_scr_p0)) + 
  geom_point() + geom_smooth()

sixth_grade %>% 
  filter(!is.na(attend_m1)) %>% 
  select(attend_m1, readng_scr_p0) %>% 
  cor()

sixth_grade %>% 
  filter(!is.na(attend_p0)) %>% 
  select(attend_p0, readng_scr_p0) %>% 
  cor()

limited_seventh_grade %>% 
  filter(attend_m1 < 1.01) %>% 
  ggplot(aes(x = attend_m1, y = readng_scr_p0)) + 
  geom_point() + geom_smooth()

limited_seventh_grade %>% 
  filter(attend_p0 < 1.01) %>% 
  ggplot(aes(x = attend_p0, y = readng_scr_p0)) + 
  geom_point() + geom_smooth()

seventh_grade %>% 
  filter(!is.na(attend_m1)) %>% 
  select(attend_m1, readng_scr_p0) %>% 
  cor()

seventh_grade %>% 
  filter(!is.na(attend_p0)) %>% 
  select(attend_p0, readng_scr_p0) %>% 
  cor()

limited_eighth_grade %>% 
  filter(attend_m1 < 1.01) %>% 
  ggplot(aes(x = attend_m1, y = readng_scr_p0)) + 
  geom_point() + geom_smooth()

limited_eighth_grade %>% 
  filter(attend_p0 < 1.01) %>% 
  ggplot(aes(x = attend_p0, y = readng_scr_p0)) + 
  geom_point() + geom_smooth()

eighth_grade %>% 
  filter(!is.na(attend_m1)) %>% 
  select(attend_m1, readng_scr_p0) %>% 
  cor()

eighth_grade %>% 
  filter(!is.na(attend_p0)) %>% 
  select(attend_p0, readng_scr_p0) %>% 
  cor()

# Dropout -- Dropout inferred doesn't really make a difference
limited_sixth_grade %>% 
  ggplot(aes(x = as.factor(dropout_inferred_p0), y = readng_scr_p0)) + 
  geom_boxplot()

score_diff(sixth_grade, dropout_inferred_p0, readng_scr_p0) # 1 has higher by 7

limited_seventh_grade %>% 
  ggplot(aes(x = as.factor(dropout_inferred_m1), y = readng_scr_p0)) + 
  geom_boxplot()

score_diff(seventh_grade, dropout_inferred_m1, readng_scr_p0) # only has 0 and NA but 0 has higher by 43

limited_seventh_grade %>% 
  ggplot(aes(x = as.factor(dropout_inferred_p0), y = readng_scr_p0)) + 
  geom_boxplot()

score_diff(seventh_grade, dropout_inferred_p0, readng_scr_p0) # 1 has higher by 4

limited_eighth_grade %>% 
  ggplot(aes(x = as.factor(dropout_inferred_m1), y = readng_scr_p0)) + 
  geom_boxplot()

score_diff(eighth_grade, dropout_inferred_m1, readng_scr_p0) # only has 0 and NA but 0 has higher by 39

limited_eighth_grade %>% 
  ggplot(aes(x = as.factor(dropout_inferred_p0), y = readng_scr_p0)) + 
  geom_boxplot()

score_diff(eighth_grade, dropout_inferred_p0, readng_scr_p0) # 1 has higher by 3

# Persist
limited_sixth_grade %>% 
  ggplot(aes(x = as.factor(persist_inferred_p0), y = readng_scr_p0)) + 
  geom_boxplot()

score_diff(sixth_grade, persist_inferred_p0, readng_scr_p0) # 0 higher by 7

limited_seventh_grade %>% 
  ggplot(aes(x = as.factor(persist_inferred_m1), y = readng_scr_p0)) + 
  geom_boxplot()

score_diff(seventh_grade, persist_inferred_m1, readng_scr_p0) # 1 vs. NA 43 (1 was higher)

limited_seventh_grade %>% 
  ggplot(aes(x = as.factor(persist_inferred_p0), y = readng_scr_p0)) + 
  geom_boxplot()

score_diff(seventh_grade, persist_inferred_p0, readng_scr_p0) # 1 has higher by 4

limited_eighth_grade %>% 
  ggplot(aes(x = as.factor(persist_inferred_m1), y = readng_scr_p0)) + 
  geom_boxplot()

score_diff(eighth_grade, persist_inferred_m1, readng_scr_p0) # 1 vs. NA 39 (1 was higher)

limited_eighth_grade %>% 
  ggplot(aes(x = as.factor(persist_inferred_p0), y = readng_scr_p0)) + 
  geom_boxplot()

score_diff(eighth_grade, persist_inferred_p0, readng_scr_p0) # 0 has higher by 3

# Transferred 
# Persist
limited_sixth_grade %>% 
  ggplot(aes(x = as.factor(transferred_out_m1), y = readng_scr_p0)) + 
  geom_boxplot()

score_diff(sixth_grade, transferred_out_m1, readng_scr_p0) # 0 higher by 33 than 1
# 1 and 2 were 2 apart 

limited_sixth_grade %>% 
  ggplot(aes(x = as.factor(transferred_out_p0), y = readng_scr_p0)) + 
  geom_boxplot()

score_diff(sixth_grade, transferred_out_p0, readng_scr_p0) # 0 higher than 1 by 37
# 1 higher than 2 by 8
# 0 higher than 2 by 40

limited_seventh_grade %>% 
  ggplot(aes(x = as.factor(transferred_out_m1), y = readng_scr_p0)) + 
  geom_boxplot()

score_diff(seventh_grade, transferred_out_m1, readng_scr_p0)
# 0 higher than 1 by 32
# 1 higher than 2 by 7 
# 2 higher than NA by 8 (NA we don't know where they transferred)

limited_seventh_grade %>% 
  ggplot(aes(x = as.factor(transferred_out_p0), y = readng_scr_p0)) + 
  geom_boxplot()

score_diff(seventh_grade, transferred_out_p0, readng_scr_p0) 
# 0 higher than 1 by 42
# 1 higher than 2 by 2
# 0 higher than 2 by 44

limited_eighth_grade %>% 
  ggplot(aes(x = as.factor(transferred_out_m1), y = readng_scr_p0)) + 
  geom_boxplot()

score_diff(eighth_grade, transferred_out_m1, readng_scr_p0) 
# 0 higher than 1 by 19 
# 1 higher than 2 by 10 
# 2 higher than NA by 13

limited_eighth_grade %>% 
  ggplot(aes(x = as.factor(transferred_out_p0), y = readng_scr_p0)) + 
  geom_boxplot()

score_diff(eighth_grade, transferred_out_p0, readng_scr_p0) 
# 0 higher than 1 by 4
# 1 higher than 2 by 22
# 0 higher than 2 by 28

# Chronic Absentee 
limited_sixth_grade %>% 
  ggplot(aes(x = as.factor(chronic_absentee_m1), y = readng_scr_p0)) + 
  geom_boxplot()

score_diff(sixth_grade, chronic_absentee_m1, readng_scr_p0) # 0 higher by 88

limited_sixth_grade %>% 
  ggplot(aes(x = as.factor(chronic_absentee_p0), y = readng_scr_p0)) + 
  geom_boxplot()

score_diff(sixth_grade, chronic_absentee_p0, readng_scr_p0) # 0 higher by 104

limited_seventh_grade %>% 
  ggplot(aes(x = as.factor(chronic_absentee_m1), y = readng_scr_p0)) + 
  geom_boxplot()

score_diff(seventh_grade, chronic_absentee_m1, readng_scr_p0) # 0 higher by 95

limited_seventh_grade %>% 
  ggplot(aes(x = as.factor(chronic_absentee_p0), y = readng_scr_p0)) + 
  geom_boxplot()

score_diff(seventh_grade, chronic_absentee_p0, readng_scr_p0) # 0 higher by 107

limited_eighth_grade %>% 
  ggplot(aes(x = as.factor(chronic_absentee_m1), y = readng_scr_p0)) + 
  geom_boxplot()

score_diff(eighth_grade, chronic_absentee_m1, readng_scr_p0) # 0 higher by 71

limited_eighth_grade %>% 
  ggplot(aes(x = as.factor(chronic_absentee_p0), y = readng_scr_p0)) + 
  geom_boxplot()

score_diff(eighth_grade, chronic_absentee_p0, readng_scr_p0) # 0 higher by 81

# Language
limited_sixth_grade %>% 
  ggplot(aes(x = as.factor(readng_lan_p0), y = readng_scr_p0)) + 
  geom_boxplot()

score_diff(sixth_grade, readng_lan_p0, readng_scr_p0) # 5 higher than 6 by 58
# 5 English, 6 Spanish 

sixth_grade %>% 
  group_by(migrant_ever, readng_lan_p0) %>% 
  summarize(perc_pop = n() / nrow(sixth_grade), 
            math_mean = mean(readng_scr_p0))

limited_seventh_grade %>% 
  ggplot(aes(x = as.factor(readng_lan_m1), y = readng_scr_p0)) + 
  geom_boxplot()

score_diff(seventh_grade, readng_lan_m1, readng_scr_p0) # 0 higher by 46 

seventh_grade %>% 
  group_by(migrant_ever, readng_lan_m1) %>% 
  summarize(perc_pop = n() / nrow(seventh_grade), 
            math_mean = mean(readng_scr_p0))
# (0, 5) got highest score 

limited_seventh_grade %>% 
  ggplot(aes(x = as.factor(readng_lan_p0), y = readng_scr_p0)) + 
  geom_boxplot()

score_diff(seventh_grade, readng_lan_p0, readng_scr_p0) # 0 higher by 77

seventh_grade %>% 
  group_by(migrant_ever, readng_lan_p0) %>% 
  summarize(perc_pop = n() / nrow(seventh_grade), 
            math_mean = mean(readng_scr_p0))

limited_eighth_grade %>% 
  ggplot(aes(x = as.factor(readng_lan_m1), y = readng_scr_p0)) + 
  geom_boxplot()
# highest again is (0, 5) pair

score_diff(eighth_grade, readng_lan_m1, readng_scr_p0) # 0 higher by 59

eighth_grade %>% 
  group_by(migrant_ever, readng_lan_m1) %>% 
  summarize(perc_pop = n() / nrow(eighth_grade), 
            math_mean = mean(readng_scr_p0))
# (0,5) pair highest

limited_eighth_grade %>% 
  ggplot(aes(x = as.factor(readng_lan_p0), y = readng_scr_p0)) + 
  geom_boxplot()

score_diff(eighth_grade, readng_lan_p0, readng_scr_p0) # 0 higher by 83

seventh_grade %>% 
  group_by(migrant_ever, readng_lan_p0) %>% 
  summarize(perc_pop = n() / nrow(seventh_grade), 
            math_mean = mean(readng_scr_p0))
# (0,5) pair highest

# Reading and Math Language Differences 
sixth_grade %>% filter(readng_lan_p0 == readng_lan_p0) %>% nrow() / sixth_grade %>% nrow()
seventh_grade %>% filter(readng_lan_m1 == readng_lan_m1) %>% nrow() / seventh_grade %>% nrow()
seventh_grade %>% filter(readng_lan_p0 == readng_lan_p0) %>% nrow() / seventh_grade %>% nrow()
eighth_grade %>% filter(readng_lan_m1 == readng_lan_m1) %>% nrow() / eighth_grade %>% nrow()
eighth_grade %>% filter(readng_lan_p0 == readng_lan_p0) %>% nrow() / eighth_grade %>% nrow()

# Also look a difference if were home last year and now not and vice versa 
seventh_grade %>% 
  filter(homeless_now != homeless_2yr) %>% 
  mutate(homeless_change = if_else(homeless_now == 1, "now", "past")) %>% 
  group_by(homeless_change) %>% 
  summarize(n = n(), 
            mean_score = mean(readng_scr_p0))
# average 1537
mean(seventh_grade$readng_scr_p0)
# average 1573

eighth_grade %>% 
  filter(homeless_now != homeless_2yr) %>% 
  mutate(homeless_change = if_else(homeless_now == 1, "now", "past")) %>% 
  group_by(homeless_change) %>% 
  summarize(n = n(), 
            mean_score = mean(readng_scr_p0))
# average score was 1679

mean(eighth_grade$readng_scr_p0) # 1701

# everyone was just past homeless 
# everyone is coming out of poverty 

# homelessness still has lasting impacts on children's performance 
