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

non_na_corr = corr_df %>% 
  filter(!is.na(correlations)) %>% 
  arrange(desc(correlations)) 

na_vars = corr_df %>% 
  filter(is.na(correlations)) %>% 
  rownames()

na_corr = function(na_var){
  corr_mat = third_grade %>% 
    filter(!is.na(!!sym(na_var))) %>% 
    select(glmath_scr_p0, !!sym(na_var)) %>% 
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
cor(third_grade$enrfay_district, third_grade$enrfay_school)

# still kind of correlated but not as extreme as district and school
third_grade %>% 
  filter(!is.na(chronic_absentee_m1), !is.na(chronic_absentee_p0)) %>% 
  select(chronic_absentee_m1, chronic_absentee_p0) %>% 
  cor()

models_list = list()

models_list[['model0']] = lmer(glmath_scr_p0 ~ 1 + (1 | schoolid_nces_enroll_p0), 
                               data = third_grade)

models_list[['model1']] = lmer(glmath_scr_p0 ~ frl_2yr + (1 | schoolid_nces_enroll_p0), 
                               data = third_grade)

models_list[['model2']] = lmer(glmath_scr_p0 ~ frl_2yr + specialed_now + (1 | schoolid_nces_enroll_p0), 
                               data = third_grade)

models_list[['model3']] = lmer(glmath_scr_p0 ~ frl_2yr + specialed_now + chronic_absentee_p0 + (1 | schoolid_nces_enroll_p0), 
                               data = third_grade)

models_list[['model4']] = lmer(glmath_scr_p0 ~ frl_2yr + specialed_now + chronic_absentee_p0 + 
                                 chronic_absentee_m1 + (1 | schoolid_nces_enroll_p0), data = third_grade)

models_list[['model5']] = lmer(glmath_scr_p0 ~ frl_2yr + specialed_now + chronic_absentee_p0 + 
                                 chronic_absentee_m1 + enrfay_school + (1 | schoolid_nces_enroll_p0), data = third_grade)

models_list[['model6']] = lmer(glmath_scr_p0 ~ frl_2yr + specialed_now + chronic_absentee_p0 + 
                                 chronic_absentee_m1 + enrfay_school + raceth + 
                                 (1 | schoolid_nces_enroll_p0), data = third_grade)

models_list[['model7']] = lmer(glmath_scr_p0 ~ frl_2yr + specialed_now + chronic_absentee_p0 + 
                                 chronic_absentee_m1 + enrfay_school + 
                                 raceth + lep_now + (1 | schoolid_nces_enroll_p0), data = third_grade)

models_list[['model8']] = lmer(glmath_scr_p0 ~ frl_2yr + specialed_now + chronic_absentee_p0 + 
                                 chronic_absentee_m1 + enrfay_school + raceth + lep_now + 
                                 homeless_2yr + (1 | schoolid_nces_enroll_p0), data = third_grade)

models_list[['model9']] = lmer(glmath_scr_p0 ~ frl_2yr + specialed_now + chronic_absentee_p0 + 
                                 chronic_absentee_m1 + enrfay_school + raceth + lep_now + 
                                 homeless_2yr + transferred_out_p0 + (1 | schoolid_nces_enroll_p0), data = third_grade)

models_list[['model10']] = lmer(glmath_scr_p0 ~ frl_2yr + specialed_now + chronic_absentee_p0 + 
                                 chronic_absentee_m1 + enrfay_school + raceth + lep_now + 
                                 homeless_2yr + transferred_out_p0 + age + (1 | schoolid_nces_enroll_p0), data = third_grade)

models_list[['model11']] = lmer(glmath_scr_p0 ~ frl_2yr + specialed_now + chronic_absentee_p0 + 
                                  chronic_absentee_m1 + enrfay_school + raceth + lep_now + 
                                  homeless_2yr + transferred_out_p0 + age + 
                                  attend_m1 + (1 | schoolid_nces_enroll_p0), data = third_grade)

models_list[['model12']] = lmer(glmath_scr_p0 ~ frl_2yr + specialed_now + chronic_absentee_p0 + 
                                  chronic_absentee_m1 + enrfay_school + raceth + lep_now + 
                                  homeless_2yr + transferred_out_p0 + age + 
                                  attend_m1 + gender + (1 | schoolid_nces_enroll_p0), data = third_grade)

models_list[['model13']] = lmer(glmath_scr_p0 ~ frl_2yr + specialed_now + chronic_absentee_p0 + 
                                  chronic_absentee_m1 + enrfay_school + raceth + lep_now + 
                                  homeless_2yr + transferred_out_p0 + age + 
                                  attend_m1 + gender + migrant_2yr + (1 | schoolid_nces_enroll_p0), data = third_grade)

models_list[['model14']] = lmer(glmath_scr_p0 ~ frl_2yr + specialed_now + chronic_absentee_p0 + 
                                  chronic_absentee_m1 + enrfay_school + raceth + lep_now + 
                                  homeless_2yr + transferred_out_p0 + age + 
                                  attend_m1 + gender + migrant_2yr + 
                                  persist_inferred_p0 + (1 | schoolid_nces_enroll_p0), data = third_grade)

models_list[['model15']] = lmer(glmath_scr_p0 ~ frl_2yr + specialed_now + chronic_absentee_p0 + 
                                  chronic_absentee_m1 + enrfay_school + raceth + lep_now + 
                                  homeless_2yr + transferred_out_p0 + age + 
                                  attend_m1 + gender + migrant_2yr + 
                                  persist_inferred_p0 + dropout_inferred_p0 + (1 | schoolid_nces_enroll_p0), data = third_grade)

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
