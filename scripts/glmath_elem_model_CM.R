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

# Creating the datasets 
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


# seeing other rows with na 
lapply(third_grade, function(x) { length(which(is.na(x)))}) 

lapply(fourth_grade, function(x) { length(which(is.na(x)))})

lapply(fifth_grade, function(x) { length(which(is.na(x)))})


# seeing correlations 
highest_corr_variables = function(df){
  cor_ind_read = function(df, root_var){
    corr_df = df %>% 
      drop_na(starts_with(root_var)) %>% 
      select(starts_with(root_var), glmath_scr_p0) %>% 
      cor() 
    return(corr_df)
  }
  
  root_vars = c('frl', 'migrant', 'lep', 'homeless', 'specialed')
  
  
  for(i in root_vars){
    cor_ind_read(df, i)
  }
  
  max_corr_math = function(df, root_var){
    var_corr = names(which.max(abs(cor_ind_read(df, root_var)[0:4, "glmath_scr_p0"])))
    cat(var_corr, "\n")
  }
  
  
  # still some issues with this 
  for(i in root_vars){
    max_corr_math(df, i)
  }
}

third_vars = highest_corr_variables(third_grade)
fourth_vars = highest_corr_variables(fourth_grade)
fifth_vars = highest_corr_variables(fifth_grade)

# for third grade frl_2yr, migrant_2yr, lep_now, homeless_2yr, specialed_now
# for fourth grade frl_2yr, migrant_2yr, lep_now, homeless_2yr, specialed_now 
# for fifth grade frl_elem, migrant_2yr, lep_now, homeless_now, specialed_now 

# ever variable type is represented here (will try with each distinct one)


numeric_third = third_grade %>% 
  select(is.numeric) 

cor(numeric_third$glmath_scr_p0, numeric_third) 

third_grade %>% colnames()

# all these variables for third grade had NO variance so removing because they won't help in the slightest 
#acadyear, gradelevel, droupout_inferred_m1, persist_inferred_m1, glmath_ver_p0, readng_ver_p0

# models with just ever 
ever_model = function(df){
  ever_model = lmer(glmath_scr_p0 ~ raceth + gender + age + 
                      frl_ever + migrant_ever + lep_ever + homeless_ever + specialed_ever + 
                      attend_m1 + attend_p0 + 
                      chronic_absentee_p0 + chronic_absentee_m1 + 
                      dropout_inferred_p0 + transferred_out_m1 + 
                      persist_inferred_p0 + transferred_out_p0 + 
                      (1 | schoolid_nces_enroll_p0), data = df)
  
  return(ever_model)
}

third_ever_model = ever_model(third_grade)
third_ever_aic = AIC(third_ever_model)
third_ever_bic = BIC(third_ever_model)

fourth_ever_model = ever_model(fourth_grade)
fourth_ever_aic = AIC(fourth_ever_model)
fourth_ever_bic = BIC(fourth_ever_model)

fifth_ever_model = ever_model(fifth_grade)
fifth_ever_aic = AIC(fifth_ever_model)
fifth_ever_bic = BIC(fifth_ever_model)

# models with just now 
now_model_aic = function(df){
  now_model = lmer(glmath_scr_p0 ~ raceth + gender + age + 
                     frl_now + migrant_now + lep_now + homeless_now + specialed_now + 
                     attend_m1 + attend_p0 + 
                     chronic_absentee_p0 + chronic_absentee_m1 + 
                     dropout_inferred_p0 + transferred_out_m1 + 
                     persist_inferred_p0 + transferred_out_p0 + 
                     (1 | schoolid_nces_enroll_p0), data = df)
  
  return(now_model)
}

third_now_model = now_model_aic(third_grade)
third_now_aic = AIC(third_now_model)
third_now_bic = BIC(third_now_model)

fourth_now_model = now_model_aic(fourth_grade)
fourth_now_aic = AIC(fourth_ever_model)
fourth_now_bic = BIC(fourth_ever_model)

fifth_now_model = now_model_aic(fifth_grade)
fifth_now_aic = AIC(fifth_ever_model)
fifth_now_bic = BIC(fifth_ever_model)

# model with essentially nothing (are we doing better than at least that?)
super_naive_third = lmer(glmath_scr_p0 ~  (1 | schoolid_nces_enroll_p0), data = third_grade)
third_naive_aic = AIC(super_naive_third)
third_naive_bic = BIC(super_naive_third)

super_naive_fourth = lmer(glmath_scr_p0 ~  (1 | schoolid_nces_enroll_p0), data = fourth_grade)
fourth_naive_aic = AIC(super_naive_fourth)
fourth_naive_bic = BIC(super_naive_fourth)

super_naive_fifth = lmer(glmath_scr_p0 ~  (1 | schoolid_nces_enroll_p0), data = fifth_grade)
fifth_naive_aic = AIC(super_naive_fifth)
fifth_naive_bic = BIC(super_naive_fifth)

#...not really performing that much better 

# taking out some past year stuff? -- spoiler: performed worse
present_model = lmer(glmath_scr_p0 ~ raceth + gender + age + 
                       frl_now + migrant_now + lep_now + homeless_now + specialed_now + attend_p0 + 
                       chronic_absentee_p0 + 
                       dropout_inferred_p0 + 
                       persist_inferred_p0 + transferred_out_p0 + 
                       (1 | schoolid_nces_enroll_p0), data = third_grade)
AIC(present_model)

# models with just 2yr 
year2_model_aic = function(df){
  now_model = lmer(glmath_scr_p0 ~ raceth + gender + age + 
                     frl_2yr + migrant_2yr + lep_2yr + homeless_2yr + specialed_2yr + 
                     attend_m1 + attend_p0 + 
                     chronic_absentee_p0 + chronic_absentee_m1 + 
                     dropout_inferred_p0 + transferred_out_m1 + 
                     persist_inferred_p0 + transferred_out_p0 + 
                     (1 | schoolid_nces_enroll_p0), data = df)
  
  return(now_model)
}

third_2yr_model = year2_model_aic(third_grade)
fourth_2yr_model = year2_model_aic(fourth_grade)
fifth_2yr_model = year2_model_aic(fifth_grade)

# models with elem
elem_model_aic = function(df){
  now_model = lmer(glmath_scr_p0 ~ raceth + gender + age + 
                     frl_elem + migrant_elem + lep_elem + homeless_elem + specialed_elem + 
                     attend_m1 + attend_p0 + 
                     chronic_absentee_p0 + chronic_absentee_m1 + 
                     dropout_inferred_p0 + transferred_out_m1 + 
                     persist_inferred_p0 + transferred_out_p0 + 
                     (1 | schoolid_nces_enroll_p0), data = df)
  
  return(now_model)
}

third_elem_model = elem_model_aic(third_grade)
fourth_elem_model = elem_model_aic(fourth_grade)
fifth_elem_model = elem_model_aic(fifth_grade)

nodem_model = function(df){
  no_demographics = lmer(glmath_scr_p0 ~ raceth + gender + age + 
                           attend_m1 + attend_p0 + 
                           chronic_absentee_p0 + chronic_absentee_m1 + 
                           dropout_inferred_p0 + transferred_out_m1 + 
                           persist_inferred_p0 + transferred_out_p0 + 
                           (1 | schoolid_nces_enroll_p0), data = df)
  
  return(no_demographics)
}

third_nodem_model = nodem_model(third_grade)
third_nodem_aic = AIC(third_nodem_model)
third_nodem_bic = BIC(third_nodem_model)

fourth_nodem_model = nodem_model(fourth_grade)
fourth_nodem_aic = AIC(fourth_nodem_model)
fourth_nodem_bic = BIC(fourth_nodem_model)

fifth_nodem_model = nodem_model(fifth_grade)
fifth_nodem_aic = AIC(fifth_nodem_model)
fifth_nodem_bic = BIC(fifth_nodem_model)

mse_func = function(model){
  return(mean(resid(model)^2))
}

model_aic_bic_df = data.frame(model_name = c(rep("Super Naive", 3), rep("No Demographics", 3), rep("Elem", 3), 
                          rep("2yr", 3), rep("Now", 3), rep("Ever", 3)), 
           grade_level = rep(c(3, 4, 5), 6),
           AIC = c(third_naive_aic, fourth_naive_aic, fourth_naive_aic, 
                   third_nodem_aic, fourth_nodem_aic, fifth_nodem_aic, 
                   AIC(third_elem_model), AIC(fourth_elem_model), AIC(fifth_elem_model), 
                   AIC(third_2yr_model), AIC(fourth_2yr_model), AIC(fifth_2yr_model), 
                   third_now_aic, fourth_now_aic, fifth_now_aic, 
                   third_ever_aic, fourth_ever_aic, fifth_ever_aic), 
           BIC = c(third_naive_bic, fourth_naive_bic, fifth_naive_bic, 
                   third_nodem_bic, fourth_nodem_bic, fifth_nodem_bic, 
                   BIC(third_elem_model), BIC(fourth_elem_model), BIC(fifth_elem_model), 
                   BIC(third_2yr_model), BIC(fourth_2yr_model), BIC(fifth_2yr_model), 
                   third_now_bic, fourth_now_bic, fifth_now_bic, 
                   third_ever_bic, fourth_ever_bic, fifth_ever_bic), 
           MSE = c(mse_func(super_naive_third), mse_func(super_naive_fourth), mse_func(super_naive_fifth), 
                   mse_func(third_nodem_model), mse_func(fourth_nodem_model), mse_func(fifth_nodem_model), 
                   mse_func(third_elem_model), mse_func(fourth_elem_model), mse_func(fifth_elem_model), 
                   mse_func(third_2yr_model), mse_func(fourth_2yr_model), mse_func(fifth_2yr_model), 
                   mse_func(third_now_model), mse_func(fourth_now_model), mse_func(fifth_now_model), 
                   mse_func(third_ever_model), mse_func(fourth_ever_model), mse_func(fifth_ever_model)))

model_aic_bic_df %>% 
  filter(grade_level == 3) %>% 
  arrange(desc(AIC)) %>% 
  head(5)

model_aic_bic_df %>% 
  filter(grade_level == 4) %>% 
  arrange(desc(AIC)) %>% 
  head(5)

model_aic_bic_df %>% 
  filter(grade_level == 5) %>% 
  arrange(desc(AIC)) %>% 
  head(5)

model_aic_bic_df %>% 
  filter(grade_level == 3) %>% 
  arrange(desc(BIC)) %>% 
  head(5)

model_aic_bic_df %>% 
  filter(grade_level == 4) %>% 
  arrange(desc(BIC)) %>% 
  head(5)

model_aic_bic_df %>% 
  filter(grade_level == 5) %>% 
  arrange(desc(BIC)) %>% 
  head(5)
