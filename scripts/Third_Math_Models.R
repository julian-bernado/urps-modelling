# Package Download 
library(dplyr)
library(readr)
library(tidyverse)
library(lme4)
library(stats)
library(metan)

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

# Don't have past year scores for third grade 

score_diff = function(var_interest){
  third_grade %>% 
    group_by(!!sym(var_interest)) %>% 
    summarize(mean_math = mean(glmath_scr_p0), 
              stan_err_math = sd(glmath_scr_p0), 
              perc_pop = n()/nrow(third_grade))
}

vars = colnames(third_grade)

summaries = map(vars, score_diff)

numeric_third = third_grade %>% 
  select(where(is.numeric)) 

correlations = cor(numeric_third[, colnames(numeric_third) != 'glmath_scr_p0'], 
    numeric_third$glmath_scr_p0)

corr_df = data.frame(correlations)

corr_df %>% 
  arrange(desc(correlations))
