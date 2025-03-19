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
eighth_grade = df %>% 
  filter(gradelevel == 8, !is.na(readng_scr_p0)) 

na_eighth_grade = data.frame(colSums(is.na(eighth_grade))) %>% 
  mutate(total_na = colSums.is.na.eighth_grade..) %>% 
  select(total_na) %>% 
  filter(total_na >= .95* nrow(eighth_grade)) %>% 
  row.names()

eighth_grade = eighth_grade %>% 
  select(!all_of(na_eighth_grade))

summary(eighth_grade)

no_variance_vars = c('readng_ver_p0', 'glmath_ver_p0', 'persist_inferred_m1', 
                     'dropout_inferred_m1', 'gradelevel', 'acadyear')

eighth_grade = eighth_grade %>% 
  select(!all_of(no_variance_vars))

# Don't have past year scores for eighth grade 

score_diff = function(var_interest){
  eighth_grade %>% 
    group_by(!!sym(var_interest)) %>% 
    summarize(mean_math = mean(readng_scr_p0), 
              stan_err_math = sd(readng_scr_p0), 
              perc_pop = n()/nrow(eighth_grade))
}

vars = colnames(eighth_grade)

summaries = map(vars, score_diff)

numeric_eighth = eighth_grade %>% 
  select(where(is.numeric)) 

correlations = cor(numeric_eighth[, colnames(numeric_eighth) != 'readng_scr_p0'], 
                   numeric_eighth$readng_scr_p0)

corr_df = data.frame(correlations)

non_na_corr = corr_df %>% 
  filter(!is.na(correlations)) %>% 
  arrange(desc(correlations)) 

na_vars = corr_df %>% 
  filter(is.na(correlations)) %>% 
  rownames()

na_corr = function(na_var){
  corr_mat = eighth_grade %>% 
    filter(!is.na(!!sym(na_var))) %>% 
    select(readng_scr_p0, !!sym(na_var)) %>% 
    cor()
  return(corr_mat[1, 2])
}

corrs = unlist(map(na_vars, na_corr))

na_corr_df = data.frame(row.names = na_vars, correlations = corrs)

corr_df = rbind(non_na_corr, na_corr_df) %>% 
  mutate(absolute_corr = abs(correlations)) %>% 
  arrange(desc(absolute_corr))

corr_df 

# highly correlated!
cor(eighth_grade$enrfay_district, eighth_grade$enrfay_school)

# still kind of correlated but not as extreme as district and school
eighth_grade %>% 
  filter(!is.na(chronic_absentee_m1), !is.na(chronic_absentee_p0)) %>% 
  select(chronic_absentee_m1, chronic_absentee_p0) %>% 
  cor()

eighth_grade %>% 
  filter(!is.na(lep_now), !is.na(readng_lan_p0)) %>% 
  select(lep_now, readng_lan_p0) %>% 
  cor()

# maybe I should add the language they've taken the exam in 

eighth_grade %>% 
  filter(!is.na(glmath_lan_p0), !is.na(readng_lan_p0)) %>% 
  select(glmath_lan_p0, readng_lan_p0) %>% 
  cor()

models_list = list()

models_list[['model0']] = lmer(readng_scr_p0 ~ 1 + (1 | schoolid_nces_enroll_p0), 
                               data = eighth_grade)

models_list[['model1']] = lmer(readng_scr_p0 ~ readng_scr_m1 + (1 | schoolid_nces_enroll_p0), 
                               data = eighth_grade)

models_list[['model2']] = lmer(readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 +  
                                 (1 | schoolid_nces_enroll_p0), data = eighth_grade)

models_list[['model3']] = lmer(readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + 
                                 (1 | schoolid_nces_enroll_p0), data = eighth_grade)

models_list[['model4']] = lmer(readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now +
                                 frl_2yr + (1 | schoolid_nces_enroll_p0), data = eighth_grade)

models_list[['model5']] = lmer(readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now +
                                 frl_2yr + lep_now + (1 | schoolid_nces_enroll_p0), data = eighth_grade)

models_list[['model6']] = lmer(readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now +
                                 frl_2yr + lep_now + chronic_absentee_p0 + (1 | schoolid_nces_enroll_p0), data = eighth_grade)

models_list[['model7']] = lmer(readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now +
                                 frl_2yr + lep_now + chronic_absentee_p0 + 
                                 age + (1 | schoolid_nces_enroll_p0), data = eighth_grade)

models_list[['model8']] = lmer(readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now +
                                 frl_2yr + lep_now + chronic_absentee_p0 + 
                                 age + chronic_absentee_m1 + (1 | schoolid_nces_enroll_p0), data = eighth_grade)

models_list[['model9']] = lmer(readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now +
                                 frl_2yr + lep_now + chronic_absentee_p0 + 
                                 age + chronic_absentee_m1 + 
                                 enrfay_school + (1 | schoolid_nces_enroll_p0), data = eighth_grade)

models_list[['model10']] = lmer(readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now +
                                  frl_2yr + lep_now + chronic_absentee_p0 + 
                                  age + chronic_absentee_m1 + 
                                  enrfay_school + raceth + (1 | schoolid_nces_enroll_p0), data = eighth_grade)

models_list[['model11']] = lmer(readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now +
                                  frl_2yr + lep_now + chronic_absentee_p0 + 
                                  age + chronic_absentee_m1 + 
                                  enrfay_school + raceth + gender + (1 | schoolid_nces_enroll_p0), data = eighth_grade)

models_list[['model12']] = lmer(readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now +
                                  frl_2yr + lep_now + chronic_absentee_p0 + 
                                  age + chronic_absentee_m1 + 
                                  enrfay_school + raceth + gender+ 
                                  attend_p0 + (1 | schoolid_nces_enroll_p0), data = eighth_grade)

models_list[['model13']] = lmer(readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now +
                                  frl_2yr + lep_now + chronic_absentee_p0 + 
                                  age + chronic_absentee_m1 + 
                                  enrfay_school + raceth + gender+ 
                                  attend_p0 + attend_m1 + (1 | schoolid_nces_enroll_p0), data = eighth_grade)

models_list[['model14']] = lmer(readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now +
                                  frl_2yr + lep_now + chronic_absentee_p0 + 
                                  age + chronic_absentee_m1 + 
                                  enrfay_school + raceth + gender+ 
                                  attend_p0 + attend_m1 + readng_lan_m1 + (1 | schoolid_nces_enroll_p0), data = eighth_grade)

models_list[['model14']] = lmer(readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now +
                                  frl_2yr + lep_now + chronic_absentee_p0 + 
                                  age + chronic_absentee_m1 + 
                                  enrfay_school + raceth + gender+ 
                                  attend_p0 + attend_m1 + readng_lan_m1 + 
                                  homeless_2yr + (1 | schoolid_nces_enroll_p0), data = eighth_grade)

models_list[['model15']] = lmer(readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now +
                                  frl_2yr + lep_now + chronic_absentee_p0 + 
                                  age + chronic_absentee_m1 + 
                                  enrfay_school + raceth + gender+ 
                                  attend_p0 + attend_m1 + readng_lan_m1 + 
                                  homeless_2yr + transferred_out_m1 + (1 | schoolid_nces_enroll_p0), data = eighth_grade)

models_list[['model16']] = lmer(readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now +
                                  frl_2yr + lep_now + chronic_absentee_p0 + 
                                  age + chronic_absentee_m1 + 
                                  enrfay_school + raceth + gender+ 
                                  attend_p0 + attend_m1 + readng_lan_m1 + 
                                  homeless_2yr + transferred_out_m1 + 
                                  transferred_out_p0 + (1 | schoolid_nces_enroll_p0), data = eighth_grade)

models_list[['model17']] = lmer(readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now +
                                  frl_2yr + lep_now + chronic_absentee_p0 + 
                                  age + chronic_absentee_m1 + 
                                  enrfay_school + raceth + gender+ 
                                  attend_p0 + attend_m1 + readng_lan_m1 + 
                                  homeless_2yr + transferred_out_m1 + 
                                  transferred_out_p0 + dropout_inferred_p0 + (1 | schoolid_nces_enroll_p0), data = eighth_grade)

models_list[['model18']] = lmer(readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now +
                                  frl_2yr + lep_now + chronic_absentee_p0 + 
                                  age + chronic_absentee_m1 + 
                                  enrfay_school + raceth + gender+ 
                                  attend_p0 + attend_m1 + readng_lan_m1 + 
                                  homeless_2yr + transferred_out_m1 + 
                                  transferred_out_p0 + dropout_inferred_p0 + 
                                  persist_inferred_p0 + (1 | schoolid_nces_enroll_p0), data = eighth_grade)

models_list[['model19']] = lmer(readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now +
                                  frl_2yr + lep_now + chronic_absentee_p0 + 
                                  age + chronic_absentee_m1 + 
                                  enrfay_school + raceth + gender+ 
                                  attend_p0 + attend_m1 + readng_lan_m1 + 
                                  homeless_2yr + transferred_out_m1 + 
                                  transferred_out_p0 + dropout_inferred_p0 + 
                                  persist_inferred_p0 + migrant_2yr + (1 | schoolid_nces_enroll_p0), data = eighth_grade)
model_summary_stat = list()

for(i in seq_along(models_list)){
  model_summary_stat[[i]] = list(model = models_list[[i]], 
                                 AIC = AIC(models_list[[i]]), 
                                 BIC = BIC(models_list[[i]]), 
                                 MSE = mean(residuals(models_list[[i]])^2))
}

AICs = data.frame(sapply(model_summary_stat, function(x) x$AIC))
BICs = data.frame(sapply(model_summary_stat, function(x) x$BIC))
MSEs = data.frame(sapply(model_summary_stat, function(x) x$MSE))

sum_stat_df = data.frame(AIC = AICs, 
                         BIC = BICs, 
                         MSE = MSEs) %>% 
  mutate(AIC = sapply.model_summary_stat..function.x..x.AIC., 
         BIC = sapply.model_summary_stat..function.x..x.BIC., 
         MSE = sapply.model_summary_stat..function.x..x.MSE.) %>% 
  select(AIC, BIC, MSE) 

sum_stat_df$model_num = as.numeric(rownames(sum_stat_df)) - 1


sum_stat_df %>% 
  ggplot(aes(x = model_num, y = AIC)) + 
  geom_point() + 
  geom_line()

sum_stat_df %>% 
  ggplot(aes(x = model_num, y = BIC)) + 
  geom_point() + 
  geom_line()

sum_stat_df %>% 
  ggplot(aes(x = model_num, y = MSE)) + 
  geom_point() + 
  geom_line()

# MSE reduced SO much just by adding first predictor -- it has a lot of power! 
