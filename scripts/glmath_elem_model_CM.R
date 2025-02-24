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

# Selecting just columns with grades 3-5 for normal test 
third_grade = df %>% 
  filter(gradelevel == 3, !is.na(glmath_scr_p0)) %>% 
  select(-readng_alt_scr_p0, -glmath_ver_p0, 
         -glmath_alt_scr_p0, -glmath_alt_scr_m1, 
         -frl_midd, -lep_midd, -migrant_midd, -homeless_midd, -specialed_midd. 
         -withdrawal_date_p0)

fourth_grade = df %>% 
  filter(gradelevel == 4, !is.na(glmath_scr_p0)) %>% 
  select(-readng_alt_scr_p0, -glmath_ver_p0, 
         -glmath_alt_scr_p0, -glmath_alt_scr_m1, 
         -frl_midd, -lep_midd, -migrant_midd, -homeless_midd, -specialed_midd)

fifth_grade = df %>% 
  filter(gradelevel == 5, !is.na(glmath_scr_p0)) %>% 
  select(-readng_alt_scr_p0, -glmath_ver_p0, -glmath_alt_scr_p0, -glmath_alt_scr_m1, 
         -frl_midd, -lep_midd, -migrant_midd, -homeless_midd, -specialed_midd)

# seeing other rows with na 
lapply(third_grade, function(x) { length(which(is.na(x)))})

lapply(fourth_grade, function(x) { length(which(is.na(x)))})

lapply(fifth_grade, function(x) { length(which(is.na(x)))})

# models with just ever 

third_grade %>% 
  colnames()

third_initial_model = lmer(glmath_scr_p0 ~ raceth + gender + frl_ever + migrant_ever + lep_ever + homeless_ever + specialed_ever + 
       attend_p0 + chronic_absentee_p0 + dropout_inferred_p0 + (1 | schoolid_nces_enroll_p0), data = third_grade)

fourth_initial_model = lmer(glmath_scr_p0 ~ raceth + gender + frl_ever + migrant_ever + lep_ever + homeless_ever + specialed_ever + 
                             attend_p0 + chronic_absentee_p0 + dropout_inferred_p0 + (1 | schoolid_nces_enroll_p0), data = fourth_grade)

fifth_initial_model = lmer(glmath_scr_p0 ~ raceth + gender + frl_ever + migrant_ever + lep_ever + homeless_ever + specialed_ever + 
                             attend_p0 + chronic_absentee_p0 + dropout_inferred_p0 + (1 | schoolid_nces_enroll_p0), data = fifth_grade)
