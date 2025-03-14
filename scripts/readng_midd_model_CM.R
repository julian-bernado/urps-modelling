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
sixth_grade = df %>% 
  filter(gradelevel == 6, !is.na(readng_scr_p0)) 

na_sixth_grade = data.frame(colSums(is.na(sixth_grade))) %>% 
  mutate(total_na = colSums.is.na.sixth_grade..) %>% 
  select(total_na) %>% 
  filter(total_na >= .95* nrow(sixth_grade)) %>% 
  row.names()

no_variance = c('acadyear', 'gradelevel', 'dropout_inferred_m1', 
                'persist_inferred_m1', 'glmath_ver_p0', 'readng_ver_p0')

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


# seeing other rows with na 
lapply(sixth_grade, function(x) { length(which(is.na(x)))}) 

lapply(seventh_grade, function(x) { length(which(is.na(x)))})

lapply(eighth_grade, function(x) { length(which(is.na(x)))})


# seeing correlations 
highest_corr_variables = function(df){
  cor_ind_read = function(df, root_var){
    corr_df = df %>% 
      drop_na(starts_with(root_var)) %>% 
      select(starts_with(root_var), readng_scr_p0) %>% 
      cor() 
    return(corr_df)
  }
  
  root_vars = c('frl', 'migrant', 'lep', 'homeless', 'specialed')
  
  
  for(i in root_vars){
    cor_ind_read(df, i)
  }
  
  max_corr_math = function(df, root_var){
    var_corr = names(which.max(abs(cor_ind_read(df, root_var)[0:4, "readng_scr_p0"])))
    cat(var_corr, "\n")
  }
  
  
  # still some issues with this 
  for(i in root_vars){
    max_corr_math(df, i)
  }
}

sixth_vars = highest_corr_variables(sixth_grade)
seventh_vars = highest_corr_variables(seventh_grade)
eighth_vars = highest_corr_variables(eighth_grade)

# for sixth grade frl_2yr, migrant_2yr, lep_now, homeless_2yr, specialed_now
# for seventh grade frl_2yr, migrant_2yr, lep_now, homeless_2yr, specialed_now 
# for eighth grade frl_elem, migrant_2yr, lep_now, homeless_now, specialed_now 

# ever variable type is represented here (will try with each distinct one)


numeric_sixth = sixth_grade %>% 
  select(is.numeric) 

cor(numeric_sixth$readng_scr_p0, numeric_sixth) 

sixth_grade %>% colnames()

# all these variables for sixth grade had NO variance so removing because they won't help in the slightest 
#acadyear, gradelevel, droupout_inferred_m1, persist_inferred_m1, glmath_ver_p0, readng_ver_p0

# models with just ever 
ever_model = function(df){
  ever_model = lmer(readng_scr_p0 ~ raceth + gender + age + 
                      frl_ever + migrant_ever + lep_ever + homeless_ever + specialed_ever + 
                      attend_m1 + attend_p0 + 
                      chronic_absentee_p0 + chronic_absentee_m1 + 
                      dropout_inferred_p0 + transferred_out_m1 + 
                      persist_inferred_p0 + transferred_out_p0 + 
                      (1 | schoolid_nces_enroll_p0), data = df)
  
  return(ever_model)
}

sixth_ever_model = ever_model(sixth_grade)
sixth_ever_aic = AIC(sixth_ever_model)
sixth_ever_bic = BIC(sixth_ever_model)

seventh_ever_model = ever_model(seventh_grade)
seventh_ever_aic = AIC(seventh_ever_model)
seventh_ever_bic = BIC(seventh_ever_model)

eighth_ever_model = ever_model(eighth_grade)
eighth_ever_aic = AIC(eighth_ever_model)
eighth_ever_bic = BIC(eighth_ever_model)

# models with just now 
now_model_aic = function(df){
  now_model = lmer(readng_scr_p0 ~ raceth + gender + age + 
                     frl_now + migrant_now + lep_now + homeless_now + specialed_now + 
                     attend_m1 + attend_p0 + 
                     chronic_absentee_p0 + chronic_absentee_m1 + 
                     dropout_inferred_p0 + transferred_out_m1 + 
                     persist_inferred_p0 + transferred_out_p0 + 
                     (1 | schoolid_nces_enroll_p0), data = df)
  
  return(now_model)
}

sixth_now_model = now_model_aic(sixth_grade)
sixth_now_aic = AIC(sixth_now_model)
sixth_now_bic = BIC(sixth_now_model)

seventh_now_model = now_model_aic(seventh_grade)
seventh_now_aic = AIC(seventh_ever_model)
seventh_now_bic = BIC(seventh_ever_model)

eighth_now_model = now_model_aic(eighth_grade)
eighth_now_aic = AIC(eighth_ever_model)
eighth_now_bic = BIC(eighth_ever_model)

# model with essentially nothing (are we doing better than at least that?)
super_naive_sixth = lmer(readng_scr_p0 ~  (1 | schoolid_nces_enroll_p0), data = sixth_grade)
sixth_naive_aic = AIC(super_naive_sixth)
sixth_naive_bic = BIC(super_naive_sixth)

super_naive_seventh = lmer(readng_scr_p0 ~  (1 | schoolid_nces_enroll_p0), data = seventh_grade)
seventh_naive_aic = AIC(super_naive_seventh)
seventh_naive_bic = BIC(super_naive_seventh)

super_naive_eighth = lmer(readng_scr_p0 ~  (1 | schoolid_nces_enroll_p0), data = eighth_grade)
eighth_naive_aic = AIC(super_naive_eighth)
eighth_naive_bic = BIC(super_naive_eighth)

#...not really performing that much better 

# taking out some past year stuff? -- spoiler: performed worse
present_model = lmer(readng_scr_p0 ~ raceth + gender + age + 
                       frl_now + migrant_now + lep_now + homeless_now + specialed_now + attend_p0 + 
                       chronic_absentee_p0 + 
                       dropout_inferred_p0 + 
                       persist_inferred_p0 + transferred_out_p0 + 
                       (1 | schoolid_nces_enroll_p0), data = sixth_grade)
AIC(present_model)

# models with just 2yr 
year2_model_aic = function(df){
  now_model = lmer(readng_scr_p0 ~ raceth + gender + age + 
                     frl_2yr + migrant_2yr + lep_2yr + homeless_2yr + specialed_2yr + 
                     attend_m1 + attend_p0 + 
                     chronic_absentee_p0 + chronic_absentee_m1 + 
                     dropout_inferred_p0 + transferred_out_m1 + 
                     persist_inferred_p0 + transferred_out_p0 + 
                     (1 | schoolid_nces_enroll_p0), data = df)
  
  return(now_model)
}

sixth_2yr_model = year2_model_aic(sixth_grade)
seventh_2yr_model = year2_model_aic(seventh_grade)
eighth_2yr_model = year2_model_aic(eighth_grade)

# models with elem
midd_model_aic = function(df){
  now_model = lmer(readng_scr_p0 ~ raceth + gender + age + 
                     frl_midd + migrant_midd + lep_midd + homeless_midd + specialed_midd + 
                     attend_m1 + attend_p0 + 
                     chronic_absentee_p0 + chronic_absentee_m1 + 
                     dropout_inferred_p0 + transferred_out_m1 + 
                     persist_inferred_p0 + transferred_out_p0 + 
                     (1 | schoolid_nces_enroll_p0), data = df)
  
  return(now_model)
}

sixth_midd_model = midd_model_aic(sixth_grade)
seventh_midd_model = midd_model_aic(seventh_grade)
eighth_midd_model = midd_model_aic(eighth_grade)

nodem_model = function(df){
  no_demographics = lmer(readng_scr_p0 ~ raceth + gender + age + 
                           attend_m1 + attend_p0 + 
                           chronic_absentee_p0 + chronic_absentee_m1 + 
                           dropout_inferred_p0 + transferred_out_m1 + 
                           persist_inferred_p0 + transferred_out_p0 + 
                           (1 | schoolid_nces_enroll_p0), data = df)
  
  return(no_demographics)
}

sixth_nodem_model = nodem_model(sixth_grade)
sixth_nodem_aic = AIC(sixth_nodem_model)
sixth_nodem_bic = BIC(sixth_nodem_model)

mse_func = function(model){
  return(mean(resid(model)^2))
}

seventh_nodem_model = nodem_model(seventh_grade)
seventh_nodem_aic = AIC(seventh_nodem_model)
seventh_nodem_bic = BIC(seventh_nodem_model)
seventh_nodem_mse = mean(resid(seventh_nodem_model)^2)

eighth_nodem_model = nodem_model(eighth_grade)
eighth_nodem_aic = AIC(eighth_nodem_model)
eighth_nodem_bic = BIC(eighth_nodem_model)
eighth_nodem_mse = mean(resid(eighth_nodem_model)^2)


model_aic_bic_df = data.frame(model_name = c(rep("Super Naive", 3), rep("No Demographics", 3), rep("Midd", 3), 
                                             rep("2yr", 3), rep("Now", 3), rep("Ever", 3)), 
                              grade_level = rep(c(6, 7, 8), 6),
                              AIC = c(sixth_naive_aic, seventh_naive_aic, seventh_naive_aic, 
                                      sixth_nodem_aic, seventh_nodem_aic, eighth_nodem_aic, 
                                      AIC(sixth_midd_model), AIC(seventh_midd_model), AIC(eighth_midd_model), 
                                      AIC(sixth_2yr_model), AIC(seventh_2yr_model), AIC(eighth_2yr_model), 
                                      sixth_now_aic, seventh_now_aic, eighth_now_aic, 
                                      sixth_ever_aic, seventh_ever_aic, eighth_ever_aic), 
                              BIC = c(sixth_naive_bic, seventh_naive_bic, eighth_naive_bic, 
                                      sixth_nodem_bic, seventh_nodem_bic, eighth_nodem_bic, 
                                      BIC(sixth_midd_model), BIC(seventh_midd_model), BIC(eighth_midd_model), 
                                      BIC(sixth_2yr_model), BIC(seventh_2yr_model), BIC(eighth_2yr_model), 
                                      sixth_now_bic, seventh_now_bic, eighth_now_bic, 
                                      sixth_ever_bic, seventh_ever_bic, eighth_ever_bic), 
                              MSE = c(mse_func(super_naive_sixth), mse_func(super_naive_seventh), mse_func(super_naive_eighth), 
                                      sixth_nodem_mse, seventh_nodem_mse, eighth_nodem_mse, 
                                      mse_func(sixth_midd_model), mse_func(seventh_midd_model), mse_func(eighth_midd_model), 
                                      mse_func(sixth_2yr_model), mse_func(seventh_2yr_model), mse_func(eighth_2yr_model), 
                                      mse_func(sixth_now_model), mse_func(seventh_now_model), mse_func(eighth_now_model), 
                                      mse_func(sixth_ever_model), mse_func(seventh_ever_model), mse_func(eighth_ever_model)))

model_aic_bic_df %>% 
  filter(grade_level == 6) %>% 
  arrange(desc(AIC)) %>% 
  head(5)

model_aic_bic_df %>% 
  filter(grade_level == 7) %>% 
  arrange(desc(AIC)) %>% 
  head(5)

model_aic_bic_df %>% 
  filter(grade_level == 8) %>% 
  arrange(desc(AIC)) %>% 
  head(5)

model_aic_bic_df %>% 
  filter(grade_level == 6) %>% 
  arrange(desc(BIC)) %>% 
  head(5)

model_aic_bic_df %>% 
  filter(grade_level == 7) %>% 
  arrange(desc(BIC)) %>% 
  head(5)

model_aic_bic_df %>% 
  filter(grade_level == 8) %>% 
  arrange(desc(BIC)) %>% 
  head(5)

