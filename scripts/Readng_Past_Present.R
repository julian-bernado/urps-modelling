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

sixth_grade = df %>% 
  filter(gradelevel == 6, !is.na(readng_scr_p0)) 

na_sixth_grade = data.frame(colSums(is.na(sixth_grade))) %>% 
  mutate(total_na = colSums.is.na.sixth_grade..) %>% 
  select(total_na) %>% 
  filter(total_na >= .95* nrow(sixth_grade)) %>% 
  row.names()

no_variance = c('acadyear', 'gradelevel', 'dropout_inferred_m1', 
                'persist_inferred_m1', 'readng_ver_p0', 'readng_ver_p0')

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

# Plots 
sixth_grade %>% 
  ggplot(aes(x = readng_scr_m1, y = readng_scr_p0)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

sixth_grade %>% 
  ggplot(aes(x = readng_scr_m1, y = readng_scr_p0)) + 
  geom_point() + 
  geom_smooth()

seventh_grade %>% 
  ggplot(aes(x = readng_scr_m1, y = readng_scr_p0)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

seventh_grade %>% 
  ggplot(aes(x = readng_scr_m1, y = readng_scr_p0)) + 
  geom_point() + 
  geom_smooth()

eighth_grade %>% 
  ggplot(aes(x = readng_scr_m1, y = readng_scr_p0)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

seventh_grade %>% 
  ggplot(aes(x = readng_scr_m1, y = readng_scr_p0)) + 
  geom_point() + 
  geom_smooth()

# Univariate Regressions 
# Sixth Grade 
basic_model_sixth = lm(readng_scr_p0 ~ readng_scr_m1, data = sixth_grade)

mse_basic_model_sixth = mean(residuals(basic_model_sixth)^2)
aic_basic_model_sixth = AIC(basic_model_sixth)
bic_basic_model_sixth = BIC(basic_model_sixth)

AIC_polynomial_list = list()
BIC_polynomial_list = list()
MSE_polynomial_list = list()

for(i in 1:10){
  model_polynomial = lm(readng_scr_p0 ~ I(readng_scr_m1^i), 
                        data = sixth_grade)
  AIC_polynomial_list[i] = AIC(model_polynomial)
  BIC_polynomial_list[i] = BIC(model_polynomial)
  MSE_polynomial_list[i] = mean(residuals(model_polynomial)^2)
}

data.frame(AIC = unlist(AIC_polynomial_list), degree_free = (1:10)) %>% 
  ggplot(aes(x = degree_free, y = AIC)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = aic_basic_model_sixth, col = 'red')

data.frame(BIC = unlist(BIC_polynomial_list), degree_free = (1:10)) %>% 
  ggplot(aes(x = degree_free, y = BIC)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = bic_basic_model_sixth, col = 'red')

data.frame(MSE = unlist(MSE_polynomial_list), degree_free = (1:10)) %>% 
  ggplot(aes(x = degree_free, y = MSE)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = mse_basic_model_sixth, col = 'red')

AIC_spline_list = list()
BIC_spline_list = list()
MSE_spline_list = list()

for(i in 1:10){
  model_spline = lm(readng_scr_p0 ~ ns(readng_scr_m1, df = i), data = sixth_grade)
  AIC_spline_list[i] = AIC(model_spline)
  BIC_spline_list[i] = BIC(model_spline)
  MSE_spline_list[i] = mean(residuals(model_spline)^2)
}

data.frame(AIC = unlist(AIC_spline_list), degree_free = (1:10)) %>% 
  ggplot(aes(x = degree_free, y = AIC)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = aic_basic_model_sixth, col = 'red')

data.frame(BIC = unlist(BIC_spline_list), degree_free = (1:10)) %>% 
  ggplot(aes(x = degree_free, y = BIC)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = bic_basic_model_sixth, col = 'red')

data.frame(MSE = unlist(MSE_spline_list), degree_free = (1:10)) %>% 
  ggplot(aes(x = degree_free, y = MSE)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = mse_basic_model_sixth, col = 'red')

# Seventh Grade
basic_model_seventh = lm(readng_scr_p0 ~ readng_scr_m1, data = seventh_grade)

mse_basic_model_seventh = mean(residuals(basic_model_seventh)^2)
aic_basic_model_seventh = AIC(basic_model_seventh)
bic_basic_model_seventh = BIC(basic_model_seventh)

AIC_polynomial_list = list()
BIC_polynomial_list = list()
MSE_polynomial_list = list()

for(i in 1:10){
  model_polynomial = lm(readng_scr_p0 ~ I(readng_scr_m1^i), 
                        data = seventh_grade)
  AIC_polynomial_list[i] = AIC(model_polynomial)
  BIC_polynomial_list[i] = BIC(model_polynomial)
  MSE_polynomial_list[i] = mean(residuals(model_polynomial)^2)
}

data.frame(AIC = unlist(AIC_polynomial_list), degree_free = (1:10)) %>% 
  ggplot(aes(x = degree_free, y = AIC)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = aic_basic_model_seventh, col = 'red')

data.frame(BIC = unlist(BIC_polynomial_list), degree_free = (1:10)) %>% 
  ggplot(aes(x = degree_free, y = BIC)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = bic_basic_model_seventh, col = 'red')

data.frame(MSE = unlist(MSE_polynomial_list), degree_free = (1:10)) %>% 
  ggplot(aes(x = degree_free, y = MSE)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = mse_basic_model_seventh, col = 'red')

AIC_spline_list = list()
BIC_spline_list = list()
MSE_spline_list = list()

for(i in 1:10){
  model_spline = lm(readng_scr_p0 ~ ns(readng_scr_m1, df = i), data = seventh_grade)
  AIC_spline_list[i] = AIC(model_spline)
  BIC_spline_list[i] = BIC(model_spline)
  MSE_spline_list[i] = mean(residuals(model_spline)^2)
}

data.frame(AIC = unlist(AIC_spline_list), degree_free = (1:10)) %>% 
  ggplot(aes(x = degree_free, y = AIC)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = aic_basic_model_seventh, col = 'red')

data.frame(BIC = unlist(BIC_spline_list), degree_free = (1:10)) %>% 
  ggplot(aes(x = degree_free, y = BIC)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = bic_basic_model_seventh, col = 'red')

data.frame(MSE = unlist(MSE_spline_list), degree_free = (1:10)) %>% 
  ggplot(aes(x = degree_free, y = MSE)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = mse_basic_model_seventh, col = 'red')

# Eighth Grade 
basic_model_eighth = lm(readng_scr_p0 ~ readng_scr_m1, data = eighth_grade)

mse_basic_model_eighth = mean(residuals(basic_model_eighth)^2)
aic_basic_model_eighth = AIC(basic_model_eighth)
bic_basic_model_eighth = BIC(basic_model_eighth)

AIC_polynomial_list = list()
BIC_polynomial_list = list()
MSE_polynomial_list = list()

for(i in 1:10){
  model_polynomial = lm(readng_scr_p0 ~ I(readng_scr_m1^i), 
                        data = eighth_grade)
  AIC_polynomial_list[i] = AIC(model_polynomial)
  BIC_polynomial_list[i] = BIC(model_polynomial)
  MSE_polynomial_list[i] = mean(residuals(model_polynomial)^2)
}

data.frame(AIC = unlist(AIC_polynomial_list), degree_free = (1:10)) %>% 
  ggplot(aes(x = degree_free, y = AIC)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = aic_basic_model_eighth, col = 'red')

data.frame(BIC = unlist(BIC_polynomial_list), degree_free = (1:10)) %>% 
  ggplot(aes(x = degree_free, y = BIC)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = bic_basic_model_eighth, col = 'red')

data.frame(MSE = unlist(MSE_polynomial_list), degree_free = (1:10)) %>% 
  ggplot(aes(x = degree_free, y = MSE)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = mse_basic_model_eighth, col = 'red')

AIC_spline_list = list()
BIC_spline_list = list()
MSE_spline_list = list()

for(i in 1:10){
  model_spline = lm(readng_scr_p0 ~ ns(readng_scr_m1, df = i), data = eighth_grade)
  AIC_spline_list[i] = AIC(model_spline)
  BIC_spline_list[i] = BIC(model_spline)
  MSE_spline_list[i] = mean(residuals(model_spline)^2)
}

data.frame(AIC = unlist(AIC_spline_list), degree_free = (1:10)) %>% 
  ggplot(aes(x = degree_free, y = AIC)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = aic_basic_model_eighth, col = 'red')

data.frame(BIC = unlist(BIC_spline_list), degree_free = (1:10)) %>% 
  ggplot(aes(x = degree_free, y = BIC)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = bic_basic_model_eighth, col = 'red')

data.frame(MSE = unlist(MSE_spline_list), degree_free = (1:10)) %>% 
  ggplot(aes(x = degree_free, y = MSE)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = mse_basic_model_eighth, col = 'red')
