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
fourth_grade = df %>% 
  filter(gradelevel == 4, !is.na(glmath_scr_p0)) 

na_fourth_grade = data.frame(colSums(is.na(fourth_grade))) %>% 
  mutate(total_na = colSums.is.na.fourth_grade..) %>% 
  select(total_na) %>% 
  filter(total_na >= .95* nrow(fourth_grade)) %>% 
  row.names()

fourth_grade = fourth_grade %>% 
  select(!all_of(na_fourth_grade))

no_variance_vars = c('readng_ver_p0', 'glmath_ver_p0', 'persist_inferred_m1', 
                     'dropout_inferred_m1', 'gradelevel', 'acadyear')

fourth_grade = fourth_grade %>% 
  select(!all_of(no_variance_vars))

# Don't have past year scores for fourth grade 

score_diff = function(var_interest){
  fourth_grade %>% 
    group_by(!!sym(var_interest)) %>% 
    summarize(mean_math = mean(glmath_scr_p0), 
              stan_err_math = sd(glmath_scr_p0), 
              perc_pop = n()/nrow(fourth_grade))
}

vars = colnames(fourth_grade)

summaries = map(vars, score_diff)

# Optional Standardization Uncomment out when want standardized results 
# fourth_grade = fourth_grade %>% 
#   select(where(is.numeric)) %>% 
#   select(-schoolid_nces_enroll_m1) %>% 
#   scale() %>% data.frame()

numeric_fourth = fourth_grade %>% 
  select(where(is.numeric)) 

correlations = cor(numeric_fourth[, colnames(numeric_fourth) != 'glmath_scr_p0'], 
                   numeric_fourth$glmath_scr_p0)

corr_df = data.frame(correlations)

non_na_corr = corr_df %>% 
  filter(!is.na(correlations)) %>% 
  arrange(desc(correlations)) 

na_vars = corr_df %>% 
  filter(is.na(correlations)) %>% 
  rownames()

na_corr = function(na_var){
  corr_mat = fourth_grade %>% 
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
cor(fourth_grade$enrfay_district, fourth_grade$enrfay_school)

# still kind of correlated but not as extreme as district and school
fourth_grade %>% 
  filter(!is.na(chronic_absentee_m1), !is.na(chronic_absentee_p0)) %>% 
  select(chronic_absentee_m1, chronic_absentee_p0) %>% 
  cor()

models_list = list()

models_list[['model0']] = lmer(glmath_scr_p0 ~ 1 + (1 | schoolid_nces_enroll_p0), 
                               data = fourth_grade)

models_list[['model1']] = lmer(glmath_scr_p0 ~ glmath_scr_m1 + (1 | schoolid_nces_enroll_p0), 
                               data = fourth_grade)

AIC_reference = AIC(models_list[['model1']])
BIC_reference = BIC(models_list[['model1']])
MSE_reference = mean(residuals(models_list[['model1']])^2)

AIC_spline_list = list()
BIC_spline_list = list()
MSE_spline_list = list()

for(i in 1:20){
  model_spline = lmer(glmath_scr_p0 ~ ns(glmath_scr_m1, df = i) + (1 | schoolid_nces_enroll_p0), 
                      data = fourth_grade)
  AIC_spline_list[i] = AIC(model_spline)
  BIC_spline_list[i] = BIC(model_spline)
  MSE_spline_list[i] = mean(residuals(model_spline)^2)
}

data.frame(AIC = unlist(AIC_spline_list), degree_free = (1:20)) %>% 
  ggplot(aes(x = degree_free, y = AIC)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = AIC_reference, col = 'red')

data.frame(BIC = unlist(BIC_spline_list), degree_free = (1:20)) %>% 
  ggplot(aes(x = degree_free, y = BIC)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = BIC_reference, col = 'red')

data.frame(MSE = unlist(MSE_spline_list), degree_free = (1:20)) %>% 
  ggplot(aes(x = degree_free, y = MSE)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = MSE_reference, col = 'red')

# five or 6 degrees probably good

models_list[['model2']] = lmer(glmath_scr_p0 ~ glmath_scr_m1 + readng_scr_m1 + 
                                 (1 | schoolid_nces_enroll_p0), data = fourth_grade)

models_list[['model3']] = lmer(glmath_scr_p0 ~ glmath_scr_m1 + readng_scr_m1 + specialed_now + 
                                 (1 | schoolid_nces_enroll_p0), data = fourth_grade)

models_list[['model4']] = lmer(glmath_scr_p0 ~ glmath_scr_m1 + readng_scr_m1 + specialed_now + 
                                 frl_2yr + (1 | schoolid_nces_enroll_p0), data = fourth_grade)

models_list[['model5']] = lmer(glmath_scr_p0 ~ glmath_scr_m1 + readng_scr_m1 + specialed_now + 
                                 frl_2yr + chronic_absentee_p0 + (1 | schoolid_nces_enroll_p0), data = fourth_grade)

models_list[['model6']] = lmer(glmath_scr_p0 ~ glmath_scr_m1 + readng_scr_m1 + specialed_now + 
                                 frl_2yr + chronic_absentee_p0 + chronic_absentee_m1 + 
                                 (1 | schoolid_nces_enroll_p0), data = fourth_grade)

models_list[['model7']] = lmer(glmath_scr_p0 ~ glmath_scr_m1 + readng_scr_m1 + specialed_now + 
                                 frl_2yr + chronic_absentee_p0 + chronic_absentee_m1 + chronic_absentee_m1 + 
                                 (1 | schoolid_nces_enroll_p0), data = fourth_grade)

models_list[['model8']] = lmer(glmath_scr_p0 ~ glmath_scr_m1 + readng_scr_m1 + specialed_now + 
                                 frl_2yr + chronic_absentee_p0 + chronic_absentee_m1 + chronic_absentee_m1 + 
                                 + age + (1 | schoolid_nces_enroll_p0), data = fourth_grade)

models_list[['model9']] = lmer(glmath_scr_p0 ~ glmath_scr_m1 + readng_scr_m1 + specialed_now + 
                                 frl_2yr + chronic_absentee_p0 + chronic_absentee_m1 + chronic_absentee_m1 + 
                                 + age + lep_now + (1 | schoolid_nces_enroll_p0), data = fourth_grade)

models_list[['model10']] = lmer(glmath_scr_p0 ~ glmath_scr_m1 + readng_scr_m1 + specialed_now + 
                                 frl_2yr + chronic_absentee_p0 + chronic_absentee_m1 + chronic_absentee_m1 + 
                                 + age + lep_now + raceth + 
                                  (1 | schoolid_nces_enroll_p0), data = fourth_grade)

models_list[['model11']] = lmer(glmath_scr_p0 ~ glmath_scr_m1 + readng_scr_m1 + specialed_now + 
                                  frl_2yr + chronic_absentee_p0 + chronic_absentee_m1 + chronic_absentee_m1 + 
                                  + age + lep_now + raceth + transferred_out_m1 + 
                                  (1 | schoolid_nces_enroll_p0), data = fourth_grade)

models_list[['model12']] = lmer(glmath_scr_p0 ~ glmath_scr_m1 + readng_scr_m1 + specialed_now + 
                                  frl_2yr + chronic_absentee_p0 + chronic_absentee_m1 + chronic_absentee_m1 + 
                                  + age + lep_now + raceth + transferred_out_m1 + attend_p0 + 
                                  (1 | schoolid_nces_enroll_p0), data = fourth_grade)

models_list[['model13']] = lmer(glmath_scr_p0 ~ glmath_scr_m1 + readng_scr_m1 + specialed_now + 
                                  frl_2yr + chronic_absentee_p0 + chronic_absentee_m1 + chronic_absentee_m1 + 
                                  + age + lep_now + raceth + transferred_out_m1 + attend_p0 + homeless_2yr + 
                                  (1 | schoolid_nces_enroll_p0), data = fourth_grade)

models_list[['model14']] = lmer(glmath_scr_p0 ~ glmath_scr_m1 + readng_scr_m1 + specialed_now + 
                                  frl_2yr + chronic_absentee_p0 + chronic_absentee_m1 + chronic_absentee_m1 + 
                                  + age + lep_now + raceth + transferred_out_m1 + attend_p0 + homeless_2yr + 
                                  + attend_m1 + (1 | schoolid_nces_enroll_p0), data = fourth_grade)

models_list[['model14']] = lmer(glmath_scr_p0 ~ glmath_scr_m1 + readng_scr_m1 + specialed_now + 
                                  frl_2yr + chronic_absentee_p0 + chronic_absentee_m1 + chronic_absentee_m1 + 
                                  + age + lep_now + raceth + transferred_out_m1 + attend_p0 + homeless_2yr + 
                                  + attend_m1 + gender + (1 | schoolid_nces_enroll_p0), data = fourth_grade)

models_list[['model15']] = lmer(glmath_scr_p0 ~ glmath_scr_m1 + readng_scr_m1 + specialed_now + 
                                  frl_2yr + chronic_absentee_p0 + chronic_absentee_m1 + chronic_absentee_m1 + 
                                  + age + lep_now + raceth + transferred_out_m1 + attend_p0 + homeless_2yr + 
                                  + attend_m1 + gender + migrant_2yr + (1 | schoolid_nces_enroll_p0), data = fourth_grade)

models_list[['model16']] = lmer(glmath_scr_p0 ~ glmath_scr_m1 + readng_scr_m1 + specialed_now + 
                                  frl_2yr + chronic_absentee_p0 + chronic_absentee_m1 + chronic_absentee_m1 + 
                                  + age + lep_now + raceth + transferred_out_m1 + attend_p0 + homeless_2yr + 
                                  + attend_m1 + gender + migrant_2yr + persist_inferred_p0 + 
                                  (1 | schoolid_nces_enroll_p0), data = fourth_grade)

models_list[['model17']] = lmer(glmath_scr_p0 ~ glmath_scr_m1 + readng_scr_m1 + specialed_now + 
                                  frl_2yr + chronic_absentee_p0 + chronic_absentee_m1 + chronic_absentee_m1 + 
                                  + age + lep_now + raceth + transferred_out_m1 + attend_p0 + homeless_2yr + 
                                  + attend_m1 + gender + migrant_2yr + persist_inferred_p0 + 
                                  + dropout_inferred_p0 + (1 | schoolid_nces_enroll_p0), data = fourth_grade)
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
