library(dplyr)
library(readr)
library(tidyverse)
library(lme4)
library(stats)

df <- read_csv("TEA_2019.csv")

cols_all_na = data.frame(colSums(is.na(df))) %>% 
  mutate(total_na = colSums.is.na.df..) %>% 
  select(total_na) %>% 
  filter(total_na == nrow(df)) %>% 
  row.names()

df = df %>% 
  select(!all_of(cols_all_na))

sixth_grade = df %>% 
  filter(gradelevel == 6)

nrow(sixth_grade)

cols_sixth = data.frame(colSums(is.na(sixth_grade))) %>% 
  mutate(total_na = colSums.is.na.sixth_grade..) %>% 
  select(total_na) 

cols_sixth %>% 
  filter(total_na == nrow(sixth_grade)) %>% 
  row.names()

cols_sixth %>% 
  filter(total_na > 10000) %>% 
  row.names()

# no columns are completely missing

sixth_grade %>% 
  filter(readng_ver_m1 != readng_ver_p0) %>% 
  nrow()

555 / nrow(sixth_grade)

# 0.1% changed versions between last year and now

(sixth_grade %>% filter(glmath_ver_m1 != readng_ver_p0) %>% nrow()) / nrow(sixth_grade)

(sixth_grade %>% filter(glmath_ver_m1 != glmath_ver_p0) %>% nrow()) / nrow(sixth_grade)

seventh_grade = df %>% 
  filter(gradelevel == 7)

cols_seventh = data.frame(colSums(is.na(seventh_grade))) %>% 
  mutate(total_na = colSums.is.na.seventh_grade..) %>% 
  select(total_na) 

cols_seventh %>% 
  filter(total_na == nrow(seventh_grade)) %>% 
  row.names()

cols_seventh %>% 
  filter(total_na > 10000) %>% 
  row.names()

cols_seventh %>% 
  filter(total_na < 10000) %>% 
  row.names()

# have 4 rows with completely missing data, all the elem variables, I guess the 6th graders could have gotten data collected from that time

seventh_grade = seventh_grade %>% 
  select(-frl_elem, -lep_elem, -migrant_elem, -homeless_elem, -specialed_elem)

switch_seventh = seventh_grade %>% 
  filter(readng_ver_m1 != readng_ver_p0) %>% 
  nrow()

switch_seventh / nrow(seventh_grade)

(seventh_grade %>% filter(glmath_ver_m1 != readng_ver_p0) %>% nrow()) / nrow(seventh_grade)

# 0.12% switched

eighth_grade = df %>% 
  filter(gradelevel == 8)

cols_eighth = data.frame(colSums(is.na(eighth_grade))) %>% 
  mutate(total_na = colSums.is.na.eighth_grade..) %>% 
  select(total_na) 

cols_eighth %>% 
  filter(total_na == nrow(eighth_grade)) %>% 
  row.names()

# again missing all the elems

cols_eighth %>% 
  filter(total_na > 10000) %>% 
  row.names()

# no rows that are complete (makes sense because of alternative test)
cols_eighth %>% 
  filter(total_na == 10000) %>% 
  row.names()

eighth_grade = eighth_grade %>% 
  select(-frl_elem, -lep_elem, -migrant_elem, -homeless_elem, -specialed_elem)

switch_eighth = eighth_grade %>% 
  filter(readng_ver_m1 != readng_ver_p0) %>% 
  nrow()

switch_eighth / nrow(eighth_grade)

(eighth_grade %>% filter(glmath_ver_m1 != readng_ver_p0) %>% nrow()) / nrow(eighth_grade)

# even smaller percentage text swapped!!!

# Making data frames select non-alternative test 
sixth_grade_norm = sixth_grade %>% 
  filter(!is.na(readng_scr_p0)) %>% 
  select(-readng_alt_scr_p0, -readng_ver_p0, -glmath_ver_p0, -glmath_scr_p0)

seventh_grade_norm = seventh_grade %>% 
  filter(!is.na(readng_scr_p0)) %>% 
  select(-readng_alt_scr_p0, -readng_ver_p0, -glmath_ver_p0, -glmath_scr_p0)

eighth_grade_norm = eighth_grade %>% 
  filter(!is.na(readng_scr_p0)) %>% 
  select(-readng_alt_scr_p0, -readng_ver_p0, -glmath_ver_p0, -glmath_scr_p0)

# looking at difference between ever and now 
sixth_grade_norm %>% 
  ggplot(aes(x = as.factor(frl_ever), y = readng_scr_p0)) +
  geom_boxplot()

frl_sixth = sixth_grade_norm %>% 
  select(starts_with('frl'), readng_scr_p0) %>% 
  mutate(frl_now = as.factor(frl_now), 
         frl_ever = as.factor(frl_ever), 
         frl_midd = as.factor(frl_midd), 
         frl_elem = as.factor(frl_elem), 
         frl_2yr = as.factor(frl_2yr))

frl_sixth %>% 
  group_by(frl_ever) %>% 
  summarize(avg_score = mean(readng_scr_p0), 
            std_score = sqrt(var(readng_scr_p0)))
  

sixth_grade_norm %>% 
  ggplot(aes(x = as.factor(frl_now), y = readng_scr_p0)) +
  geom_boxplot()

sixth_grade_norm %>% 
  ggplot(aes(x = as.factor(frl_midd), y = readng_scr_p0)) +
  geom_boxplot()

sixth_grade_norm %>% 
  ggplot(aes(x = as.factor(frl_elem), y = readng_scr_p0)) +
  geom_boxplot()

sixth_grade_norm %>% 
  ggplot(aes(x = as.factor(frl_2yr), y = readng_scr_p0)) +
  geom_boxplot()


# connection between frl and homeless
(sixth_grade_norm %>% filter(frl_ever == homeless_ever) %>% nrow()) / nrow(sixth_grade_norm)
# 43.96% of individuals who used free reduced lunch ever also were homeless at some point 

# connection between migrant and lep
(sixth_grade_norm %>% filter(migrant_ever == lep_ever) %>% nrow()) / nrow(sixth_grade_norm)
# 78.24% of individuals who were a migrant also had lep (these are highly correlated)

# ever models 
sixth_initial_model = lmer(readng_scr_p0 ~ raceth + gender + frl_ever + migrant_ever + lep_ever + homeless_ever + specialed_ever + 
                             attend_p0 + chronic_absentee_p0 + dropout_inferred_p0 + (1 | schoolid_nces_enroll_p0), data = sixth_grade_norm)

seventh_initial_model = lmer(readng_scr_p0 ~ raceth + gender + frl_ever + migrant_ever + lep_ever + homeless_ever + specialed_ever + 
                             attend_p0 + chronic_absentee_p0 + dropout_inferred_p0 + (1 | schoolid_nces_enroll_p0), data = seventh_grade_norm)

eighth_initial_model = lmer(readng_scr_p0 ~ raceth + gender + frl_ever + migrant_ever + lep_ever + homeless_ever + specialed_ever + 
                             attend_p0 + chronic_absentee_p0 + dropout_inferred_p0 + (1 | schoolid_nces_enroll_p0), data = eighth_grade_norm)
  
# now models 
sixth_now_model = lmer(readng_scr_p0 ~ raceth + gender + frl_now + migrant_now + lep_now + homeless_now + specialed_now + 
                             attend_p0 + chronic_absentee_p0 + dropout_inferred_p0 + (1 | schoolid_nces_enroll_p0), data = sixth_grade_norm)

seventh_now_model = lmer(readng_scr_p0 ~ raceth + gender + frl_now + migrant_now + lep_now + homeless_now + specialed_now + 
                               attend_p0 + chronic_absentee_p0 + dropout_inferred_p0 + (1 | schoolid_nces_enroll_p0), data = seventh_grade_norm)

eighth_now_model = lmer(readng_scr_p0 ~ raceth + gender + frl_now + migrant_now + lep_now + homeless_now + specialed_now + 
                              attend_p0 + chronic_absentee_p0 + dropout_inferred_p0 + (1 | schoolid_nces_enroll_p0), data = eighth_grade_norm)

# AIC 
model <- lmer(readng_scr_p0 ~ (1 | schoolid_nces_enroll_p0), data = sixth_grade_norm)
stepwise_model <- step(model)

# BIC

# MSE of models 
