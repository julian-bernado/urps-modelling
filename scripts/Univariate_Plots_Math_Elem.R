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

# Reading Score from Year Before 

limited_fourth_grade %>% 
  ggplot(aes(x = glmath_scr_m1, y = glmath_scr_p0)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

limited_fourth_grade %>% 
  ggplot(aes(x = glmath_scr_m1, y = glmath_scr_p0)) + 
  geom_point() + 
  geom_smooth()

limited_fifth_grade %>% 
  ggplot(aes(x = glmath_scr_m1, y = glmath_scr_p0)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

limited_fifth_grade %>% 
  ggplot(aes(x = glmath_scr_m1, y = glmath_scr_p0)) + 
  geom_point() + 
  geom_smooth()

## 0.729 correlation
fourth_grade %>% 
  filter(!is.na(glmath_scr_m1)) %>% 
  select(glmath_scr_m1, glmath_scr_p0) %>% 
  cor()

## 0.598 correlation
fifth_grade %>% 
  filter(!is.na(glmath_scr_m1)) %>% 
  select(glmath_scr_m1, glmath_scr_p0) %>% 
  cor()

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

# 0.611
fourth_grade %>% 
  filter(!is.na(readng_scr_m1)) %>% 
  select(readng_scr_m1, glmath_scr_p0) %>% 
  cor()

# Correlation 0.476
fifth_grade %>% 
  filter(!is.na(readng_scr_m1)) %>% 
  select(readng_scr_m1, glmath_scr_p0) %>% 
  cor()

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
score_diff(third_grade, frl_now, glmath_scr_p0)
score_diff(third_grade, frl_ever, glmath_scr_p0) # ever and elem are the same here 
score_diff(third_grade, frl_2yr, glmath_scr_p0)
score_diff(third_grade, frl_elem, glmath_scr_p0)

score_diff(fourth_grade, frl_now, glmath_scr_p0)
score_diff(fourth_grade, frl_ever, glmath_scr_p0) # ever and elem are the same here 
score_diff(fourth_grade, frl_2yr, glmath_scr_p0)
score_diff(fourth_grade, frl_elem, glmath_scr_p0)

score_diff(fifth_grade, frl_now, glmath_scr_p0)
score_diff(fifth_grade, frl_ever, glmath_scr_p0) # ever and elem are the same here 
score_diff(fifth_grade, frl_2yr, glmath_scr_p0)
score_diff(fifth_grade, frl_elem, glmath_scr_p0)

# LEP

## 0 performing a little bit better (for each grade)  
score_diff(third_grade, lep_now, glmath_scr_p0)
score_diff(third_grade, lep_ever, glmath_scr_p0) 
score_diff(third_grade, lep_2yr, glmath_scr_p0)
score_diff(third_grade, lep_elem, glmath_scr_p0)

score_diff(fourth_grade, lep_now, glmath_scr_p0)
score_diff(fourth_grade, lep_ever, glmath_scr_p0) 
score_diff(fourth_grade, lep_2yr, glmath_scr_p0)
score_diff(fourth_grade, lep_elem, glmath_scr_p0)

score_diff(fifth_grade, lep_now, glmath_scr_p0)
score_diff(fifth_grade, lep_ever, glmath_scr_p0) 
score_diff(fifth_grade, lep_2yr, glmath_scr_p0)
score_diff(fifth_grade, lep_elem, glmath_scr_p0)

# MIGRANT
## 1475 - 1433 = 42 difference normally 
score_diff(third_grade, migrant_now, glmath_scr_p0)
score_diff(third_grade, migrant_ever, glmath_scr_p0)  
score_diff(third_grade, migrant_2yr, glmath_scr_p0)
score_diff(third_grade, migrant_elem, glmath_scr_p0)

# 1573 - 1528 = 45
score_diff(fourth_grade, migrant_now, glmath_scr_p0)
score_diff(fourth_grade, migrant_ever, glmath_scr_p0)  
score_diff(fourth_grade, migrant_2yr, glmath_scr_p0)
score_diff(fourth_grade, migrant_elem, glmath_scr_p0)

# 1702 - 1664 = 38 diff 
score_diff(fifth_grade, migrant_now, glmath_scr_p0)
score_diff(fifth_grade, migrant_ever, glmath_scr_p0) 
score_diff(fifth_grade, migrant_2yr, glmath_scr_p0)
score_diff(fifth_grade, migrant_elem, glmath_scr_p0)

# HOMELESS 

## 1478 - 1426 = 52 difference normally 
score_diff(third_grade, homeless_now, glmath_scr_p0)
score_diff(third_grade, homeless_ever, glmath_scr_p0)  
score_diff(third_grade, homeless_2yr, glmath_scr_p0)
score_diff(third_grade, homeless_elem, glmath_scr_p0)

# 1574 - 1523 = 51
score_diff(fourth_grade, homeless_now, glmath_scr_p0)
score_diff(fourth_grade, homeless_ever, glmath_scr_p0)  
score_diff(fourth_grade, homeless_2yr, glmath_scr_p0)
score_diff(fourth_grade, homeless_elem, glmath_scr_p0)

# 1702 - 1664 = 38 diff 
score_diff(fifth_grade, homeless_now, glmath_scr_p0) # big impact 1703 - 1634 = 69
score_diff(fifth_grade, homeless_ever, glmath_scr_p0) # 38
score_diff(fifth_grade, homeless_2yr, glmath_scr_p0) # 39
score_diff(fifth_grade, homeless_elem, glmath_scr_p0) # 38

# SPECIALED 
# 0 is scoring much higher 
score_diff(third_grade, specialed_now, glmath_scr_p0) # 127
score_diff(third_grade, specialed_ever, glmath_scr_p0)  # 122
score_diff(third_grade, specialed_2yr, glmath_scr_p0) # 123
score_diff(third_grade, specialed_elem, glmath_scr_p0) # 122

score_diff(fourth_grade, specialed_now, glmath_scr_p0) # 135
score_diff(fourth_grade, specialed_ever, glmath_scr_p0) # 127
score_diff(fourth_grade, specialed_2yr, glmath_scr_p0) # 129
score_diff(fourth_grade, specialed_elem, glmath_scr_p0) # 127

score_diff(fifth_grade, specialed_now, glmath_scr_p0) # 118
score_diff(fifth_grade, specialed_ever, glmath_scr_p0) # 109
score_diff(fifth_grade, specialed_2yr, glmath_scr_p0) # 110
score_diff(fifth_grade, specialed_elem, glmath_scr_p0) # 109

# Attendance -- have pretty low correlation and the plot doesn't show much 
# all correlations 0.07 to 0.03
limited_third_grade %>% 
  filter(attend_m1 < 1.01) %>% 
  ggplot(aes(x = attend_m1, y = glmath_scr_p0)) + 
  geom_point() + geom_smooth()

limited_third_grade %>% 
  filter(attend_p0 < 1.01) %>% 
  ggplot(aes(x = attend_p0, y = glmath_scr_p0)) + 
  geom_point() + geom_smooth()
  
third_grade %>% 
  filter(!is.na(attend_m1)) %>% 
  select(attend_m1, glmath_scr_p0) %>% 
  cor()

third_grade %>% 
  filter(!is.na(attend_p0)) %>% 
  select(attend_p0, glmath_scr_p0) %>% 
  cor()

limited_fourth_grade %>% 
  filter(attend_m1 < 1.01) %>% 
  ggplot(aes(x = attend_m1, y = glmath_scr_p0)) + 
  geom_point() + geom_smooth()

limited_fourth_grade %>% 
  filter(attend_p0 < 1.01) %>% 
  ggplot(aes(x = attend_p0, y = glmath_scr_p0)) + 
  geom_point() + geom_smooth()

fourth_grade %>% 
  filter(!is.na(attend_m1)) %>% 
  select(attend_m1, glmath_scr_p0) %>% 
  cor()

fourth_grade %>% 
  filter(!is.na(attend_p0)) %>% 
  select(attend_p0, glmath_scr_p0) %>% 
  cor()

limited_fifth_grade %>% 
  filter(attend_m1 < 1.01) %>% 
  ggplot(aes(x = attend_m1, y = glmath_scr_p0)) + 
  geom_point() + geom_smooth()

limited_fifth_grade %>% 
  filter(attend_p0 < 1.01) %>% 
  ggplot(aes(x = attend_p0, y = glmath_scr_p0)) + 
  geom_point() + geom_smooth()

fifth_grade %>% 
  filter(!is.na(attend_m1)) %>% 
  select(attend_m1, glmath_scr_p0) %>% 
  cor()

fifth_grade %>% 
  filter(!is.na(attend_p0)) %>% 
  select(attend_p0, glmath_scr_p0) %>% 
  cor()

# Dropout -- Dropout inferred doesn't really make a difference
limited_third_grade %>% 
  ggplot(aes(x = as.factor(dropout_inferred_p0), y = glmath_scr_p0)) + 
  geom_boxplot()

score_diff(third_grade, dropout_inferred_p0, glmath_scr_p0) # 1 has higher by 7

limited_fourth_grade %>% 
  ggplot(aes(x = as.factor(dropout_inferred_m1), y = glmath_scr_p0)) + 
  geom_boxplot()

score_diff(fourth_grade, dropout_inferred_m1, glmath_scr_p0) # only has 0 and NA but 0 has higher by 43

limited_fourth_grade %>% 
  ggplot(aes(x = as.factor(dropout_inferred_p0), y = glmath_scr_p0)) + 
  geom_boxplot()

score_diff(fourth_grade, dropout_inferred_p0, glmath_scr_p0) # 1 has higher by 4

limited_fifth_grade %>% 
  ggplot(aes(x = as.factor(dropout_inferred_m1), y = glmath_scr_p0)) + 
  geom_boxplot()

score_diff(fifth_grade, dropout_inferred_m1, glmath_scr_p0) # only has 0 and NA but 0 has higher by 39

limited_fifth_grade %>% 
  ggplot(aes(x = as.factor(dropout_inferred_p0), y = glmath_scr_p0)) + 
  geom_boxplot()

score_diff(fifth_grade, dropout_inferred_p0, glmath_scr_p0) # 1 has higher by 3

# Persist
limited_third_grade %>% 
  ggplot(aes(x = as.factor(persist_inferred_p0), y = glmath_scr_p0)) + 
  geom_boxplot()

score_diff(third_grade, persist_inferred_p0, glmath_scr_p0) # 0 higher by 7

limited_fourth_grade %>% 
  ggplot(aes(x = as.factor(persist_inferred_m1), y = glmath_scr_p0)) + 
  geom_boxplot()

score_diff(fourth_grade, persist_inferred_m1, glmath_scr_p0) # 1 vs. NA 43 (1 was higher)

limited_fourth_grade %>% 
  ggplot(aes(x = as.factor(persist_inferred_p0), y = glmath_scr_p0)) + 
  geom_boxplot()

score_diff(fourth_grade, persist_inferred_p0, glmath_scr_p0) # 1 has higher by 4

limited_fifth_grade %>% 
  ggplot(aes(x = as.factor(persist_inferred_m1), y = glmath_scr_p0)) + 
  geom_boxplot()

score_diff(fifth_grade, persist_inferred_m1, glmath_scr_p0) # 1 vs. NA 39 (1 was higher)

limited_fifth_grade %>% 
  ggplot(aes(x = as.factor(persist_inferred_p0), y = glmath_scr_p0)) + 
  geom_boxplot()

score_diff(fifth_grade, persist_inferred_p0, glmath_scr_p0) # 0 has higher by 3

# Transferred 
# Persist
limited_third_grade %>% 
  ggplot(aes(x = as.factor(transferred_out_m1), y = glmath_scr_p0)) + 
  geom_boxplot()

score_diff(third_grade, transferred_out_m1, glmath_scr_p0) # 0 higher by 33 than 1
# 1 and 2 were 2 apart 

limited_third_grade %>% 
  ggplot(aes(x = as.factor(transferred_out_p0), y = glmath_scr_p0)) + 
  geom_boxplot()

score_diff(third_grade, transferred_out_p0, glmath_scr_p0) # 0 higher than 1 by 37
# 1 higher than 2 by 8
# 0 higher than 2 by 40

limited_fourth_grade %>% 
  ggplot(aes(x = as.factor(transferred_out_m1), y = glmath_scr_p0)) + 
  geom_boxplot()

score_diff(fourth_grade, transferred_out_m1, glmath_scr_p0)
# 0 higher than 1 by 32
# 1 higher than 2 by 7 
# 2 higher than NA by 8 (NA we don't know where they transferred)

limited_fourth_grade %>% 
  ggplot(aes(x = as.factor(transferred_out_p0), y = glmath_scr_p0)) + 
  geom_boxplot()

score_diff(fourth_grade, transferred_out_p0, glmath_scr_p0) 
# 0 higher than 1 by 42
# 1 higher than 2 by 2
# 0 higher than 2 by 44

limited_fifth_grade %>% 
  ggplot(aes(x = as.factor(transferred_out_m1), y = glmath_scr_p0)) + 
  geom_boxplot()

score_diff(fifth_grade, transferred_out_m1, glmath_scr_p0) 
# 0 higher than 1 by 19 
# 1 higher than 2 by 10 
# 2 higher than NA by 13

limited_fifth_grade %>% 
  ggplot(aes(x = as.factor(transferred_out_p0), y = glmath_scr_p0)) + 
  geom_boxplot()

score_diff(fifth_grade, transferred_out_p0, glmath_scr_p0) 
# 0 higher than 1 by 4
# 1 higher than 2 by 22
# 0 higher than 2 by 28

# Chronic Absentee 
limited_third_grade %>% 
  ggplot(aes(x = as.factor(chronic_absentee_m1), y = glmath_scr_p0)) + 
  geom_boxplot()

score_diff(third_grade, chronic_absentee_m1, glmath_scr_p0) # 0 higher by 88

limited_third_grade %>% 
  ggplot(aes(x = as.factor(chronic_absentee_p0), y = glmath_scr_p0)) + 
  geom_boxplot()

score_diff(third_grade, chronic_absentee_p0, glmath_scr_p0) # 0 higher by 104

limited_fourth_grade %>% 
  ggplot(aes(x = as.factor(chronic_absentee_m1), y = glmath_scr_p0)) + 
  geom_boxplot()

score_diff(fourth_grade, chronic_absentee_m1, glmath_scr_p0) # 0 higher by 95

limited_fourth_grade %>% 
  ggplot(aes(x = as.factor(chronic_absentee_p0), y = glmath_scr_p0)) + 
  geom_boxplot()

score_diff(fourth_grade, chronic_absentee_p0, glmath_scr_p0) # 0 higher by 107

limited_fifth_grade %>% 
  ggplot(aes(x = as.factor(chronic_absentee_m1), y = glmath_scr_p0)) + 
  geom_boxplot()

score_diff(fifth_grade, chronic_absentee_m1, glmath_scr_p0) # 0 higher by 71

limited_fifth_grade %>% 
  ggplot(aes(x = as.factor(chronic_absentee_p0), y = glmath_scr_p0)) + 
  geom_boxplot()

score_diff(fifth_grade, chronic_absentee_p0, glmath_scr_p0) # 0 higher by 81

# Language
limited_third_grade %>% 
  ggplot(aes(x = as.factor(glmath_lan_p0), y = glmath_scr_p0)) + 
  geom_boxplot()

score_diff(third_grade, glmath_lan_p0, glmath_scr_p0) # 5 higher than 6 by 58
# 5 English, 6 Spanish 

third_grade %>% 
  group_by(migrant_ever, glmath_lan_p0) %>% 
  summarize(perc_pop = n() / nrow(third_grade), 
            math_mean = mean(glmath_scr_p0))

limited_fourth_grade %>% 
  ggplot(aes(x = as.factor(glmath_lan_m1), y = glmath_scr_p0)) + 
  geom_boxplot()

score_diff(fourth_grade, glmath_lan_m1, glmath_scr_p0) # 0 higher by 46 

fourth_grade %>% 
  group_by(migrant_ever, glmath_lan_m1) %>% 
  summarize(perc_pop = n() / nrow(fourth_grade), 
            math_mean = mean(glmath_scr_p0))
# (0, 5) got highest score 

limited_fourth_grade %>% 
  ggplot(aes(x = as.factor(glmath_lan_p0), y = glmath_scr_p0)) + 
  geom_boxplot()

score_diff(fourth_grade, glmath_lan_p0, glmath_scr_p0) # 0 higher by 77

fourth_grade %>% 
  group_by(migrant_ever, glmath_lan_p0) %>% 
  summarize(perc_pop = n() / nrow(fourth_grade), 
            math_mean = mean(glmath_scr_p0))

limited_fifth_grade %>% 
  ggplot(aes(x = as.factor(glmath_lan_m1), y = glmath_scr_p0)) + 
  geom_boxplot()
# highest again is (0, 5) pair

score_diff(fifth_grade, glmath_lan_m1, glmath_scr_p0) # 0 higher by 59

fifth_grade %>% 
  group_by(migrant_ever, glmath_lan_m1) %>% 
  summarize(perc_pop = n() / nrow(fifth_grade), 
            math_mean = mean(glmath_scr_p0))
# (0,5) pair highest

limited_fifth_grade %>% 
  ggplot(aes(x = as.factor(glmath_lan_p0), y = glmath_scr_p0)) + 
  geom_boxplot()

score_diff(fifth_grade, glmath_lan_p0, glmath_scr_p0) # 0 higher by 83

fourth_grade %>% 
  group_by(migrant_ever, glmath_lan_p0) %>% 
  summarize(perc_pop = n() / nrow(fourth_grade), 
            math_mean = mean(glmath_scr_p0))
# (0,5) pair highest

# Reading and Math Language Differences 
third_grade %>% filter(readng_lan_p0 == glmath_lan_p0) %>% nrow() / third_grade %>% nrow()
fourth_grade %>% filter(readng_lan_m1 == glmath_lan_m1) %>% nrow() / fourth_grade %>% nrow()
fourth_grade %>% filter(readng_lan_p0 == glmath_lan_p0) %>% nrow() / fourth_grade %>% nrow()
fifth_grade %>% filter(readng_lan_m1 == glmath_lan_m1) %>% nrow() / fifth_grade %>% nrow()
fifth_grade %>% filter(readng_lan_p0 == glmath_lan_p0) %>% nrow() / fifth_grade %>% nrow()

# Also look a difference if were home last year and now not and vice versa 
fourth_grade %>% 
  filter(homeless_now != homeless_2yr) %>% 
  mutate(homeless_change = if_else(homeless_now == 1, "now", "past")) %>% 
  group_by(homeless_change) %>% 
  summarize(n = n(), 
            mean_score = mean(glmath_scr_p0))
# average 1537
mean(fourth_grade$glmath_scr_p0)
# average 1573

fifth_grade %>% 
  filter(homeless_now != homeless_2yr) %>% 
  mutate(homeless_change = if_else(homeless_now == 1, "now", "past")) %>% 
  group_by(homeless_change) %>% 
  summarize(n = n(), 
            mean_score = mean(glmath_scr_p0))
# average score was 1679

mean(fifth_grade$glmath_scr_p0) # 1701

# everyone was just past homeless 
# everyone is coming out of poverty 

# homelessness still has lasting impacts on children's performance 
