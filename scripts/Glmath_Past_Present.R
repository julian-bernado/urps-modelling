library(dplyr)
library(readr)
library(tidyverse)
library(lme4)
library(stats)
library(splines)

# Data set Download 
df <- read_csv("TEA_2019.csv")

# Preprocessing 
cols_all_na = data.frame(colSums(is.na(df))) %>% 
  mutate(total_na = colSums.is.na.df..) %>% 
  select(total_na) %>% 
  filter(total_na == nrow(df)) %>% 
  row.names()

no_variance = c('acadyear', 'gradelevel', 'dropout_inferred_m1', 
                'persist_inferred_m1', 'glmath_ver_p0', 'readng_ver_p0')

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

# Plots 
fourth_grade %>% 
  ggplot(aes(x = glmath_scr_m1, y = glmath_scr_p0)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

fourth_grade %>% 
  ggplot(aes(x = glmath_scr_m1, y = glmath_scr_p0)) + 
  geom_point() + 
  geom_smooth()

fifth_grade %>% 
  ggplot(aes(x = glmath_scr_m1, y = glmath_scr_p0)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

fourth_grade %>% 
  ggplot(aes(x = glmath_scr_m1, y = glmath_scr_p0)) + 
  geom_point() + 
  geom_smooth()

# Univariate Regressions 
# Fourth Grade
basic_model_fourth = lm(glmath_scr_p0 ~ glmath_scr_m1, data = fourth_grade)

mse_basic_model_fourth = mean(residuals(basic_model_fourth)^2)
aic_basic_model_fourth = AIC(basic_model_fourth)
bic_basic_model_fourth = BIC(basic_model_fourth)

AIC_polynomial_list = list()
BIC_polynomial_list = list()
MSE_polynomial_list = list()

for(i in 1:10){
  model_polynomial = lm(glmath_scr_p0 ~ I(glmath_scr_m1^i), 
                      data = fourth_grade)
  AIC_polynomial_list[i] = AIC(model_polynomial)
  BIC_polynomial_list[i] = BIC(model_polynomial)
  MSE_polynomial_list[i] = mean(residuals(model_polynomial)^2)
}

data.frame(AIC = unlist(AIC_polynomial_list), degree_free = (1:10)) %>% 
  ggplot(aes(x = degree_free, y = AIC)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = aic_basic_model_fourth, col = 'red')

data.frame(BIC = unlist(BIC_polynomial_list), degree_free = (1:10)) %>% 
  ggplot(aes(x = degree_free, y = BIC)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = bic_basic_model_fourth, col = 'red')

data.frame(MSE = unlist(MSE_polynomial_list), degree_free = (1:10)) %>% 
  ggplot(aes(x = degree_free, y = MSE)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = mse_basic_model_fourth, col = 'red')

AIC_spline_list = list()
BIC_spline_list = list()
MSE_spline_list = list()

for(i in 1:10){
  model_spline = lm(glmath_scr_p0 ~ ns(glmath_scr_m1, df = i), data = fourth_grade)
  AIC_spline_list[i] = AIC(model_spline)
  BIC_spline_list[i] = BIC(model_spline)
  MSE_spline_list[i] = mean(residuals(model_spline)^2)
}

data.frame(AIC = unlist(AIC_spline_list), degree_free = (1:10)) %>% 
  ggplot(aes(x = degree_free, y = AIC)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = aic_basic_model_fourth, col = 'red')

data.frame(BIC = unlist(BIC_spline_list), degree_free = (1:10)) %>% 
  ggplot(aes(x = degree_free, y = BIC)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = bic_basic_model_fourth, col = 'red')

data.frame(MSE = unlist(MSE_spline_list), degree_free = (1:10)) %>% 
  ggplot(aes(x = degree_free, y = MSE)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = mse_basic_model_fourth, col = 'red')

# Fifth Grade 
basic_model_fifth = lm(glmath_scr_p0 ~ glmath_scr_m1, data = fifth_grade)

mse_basic_model_fifth = mean(residuals(basic_model_fifth)^2)
aic_basic_model_fifth = AIC(basic_model_fifth)
bic_basic_model_fifth = BIC(basic_model_fifth)

AIC_polynomial_list = list()
BIC_polynomial_list = list()
MSE_polynomial_list = list()

for(i in 1:10){
  model_polynomial = lm(glmath_scr_p0 ~ I(glmath_scr_m1^i), 
                        data = fifth_grade)
  AIC_polynomial_list[i] = AIC(model_polynomial)
  BIC_polynomial_list[i] = BIC(model_polynomial)
  MSE_polynomial_list[i] = mean(residuals(model_polynomial)^2)
}

data.frame(AIC = unlist(AIC_polynomial_list), degree_free = (1:10)) %>% 
  ggplot(aes(x = degree_free, y = AIC)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = aic_basic_model_fifth, col = 'red')

data.frame(BIC = unlist(BIC_polynomial_list), degree_free = (1:10)) %>% 
  ggplot(aes(x = degree_free, y = BIC)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = bic_basic_model_fifth, col = 'red')

data.frame(MSE = unlist(MSE_polynomial_list), degree_free = (1:10)) %>% 
  ggplot(aes(x = degree_free, y = MSE)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = mse_basic_model_fifth, col = 'red')

AIC_spline_list = list()
BIC_spline_list = list()
MSE_spline_list = list()

for(i in 1:10){
  model_spline = lm(glmath_scr_p0 ~ ns(glmath_scr_m1, df = i), data = fifth_grade)
  AIC_spline_list[i] = AIC(model_spline)
  BIC_spline_list[i] = BIC(model_spline)
  MSE_spline_list[i] = mean(residuals(model_spline)^2)
}

data.frame(AIC = unlist(AIC_spline_list), degree_free = (1:10)) %>% 
  ggplot(aes(x = degree_free, y = AIC)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = aic_basic_model_fifth, col = 'red')

data.frame(BIC = unlist(BIC_spline_list), degree_free = (1:10)) %>% 
  ggplot(aes(x = degree_free, y = BIC)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = bic_basic_model_fifth, col = 'red')

data.frame(MSE = unlist(MSE_spline_list), degree_free = (1:10)) %>% 
  ggplot(aes(x = degree_free, y = MSE)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = mse_basic_model_fifth, col = 'red')
