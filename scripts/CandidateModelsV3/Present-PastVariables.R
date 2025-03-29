#This file experiments on new variables. 
#For simplicity, we will not scale variables at first. 
################################################################################
# Data
set.seed(489)
library(readr)
library(dplyr)
library(lme4)
library(purrr)
library(ggplot2)
data <- read_csv("/home/rstudio/TEA_2019.csv")
unique_schools <- unique(data$schoolid_nces_enroll_p0)
schools_sampled <- sample(unique_schools, size = round(0.3 * length(unique_schools)))
df <- data %>% 
  filter(schoolid_nces_enroll_p0 %in% schools_sampled) %>% 
  # Remove variables with only NA's
  select(where(~ !all(is.na(.)))) %>% 
  # Remove irrelevant math scores.
  select(-c('glmath_ver_p0', 
            'glmath_lan_p0', 
            'glmath_scr_p0', 
            'glmath_alt_scr_m1', 'glmath_alt_scr_p0',
            'readng_alt_scr_m1', 'readng_alt_scr_p0')) %>% 
  # Remove those ends by "_midd"
  select(-ends_with("_midd")) %>% 
  select(-c('replacement_id', 'acadyear')) %>% 
  # Remove those didn't have exam scores
  filter(!is.na(readng_scr_p0), gradelevel == 4) %>% 
  mutate(age_int = round(age), 
         attend_p0_d1 = round(attend_p0, 1),
         attend_m1_d1 = round(attend_m1, 1)) %>% 
  filter(!is.na(readng_scr_m1), !is.na(glmath_scr_m1), !is.na(readng_scr_p0))

df_scaled <- df
predictors_noscale <- c('districtid_nces_enroll_m1', 'districtid_nces_enroll_p0', 
                        'schoolid_nces_enroll_m1', 'schoolid_nces_enroll_p0','gradelevel')
# Assuming predictors_noscale is a vector of column names you don't want to scale
df_scaled[ , setdiff(names(df), predictors_noscale)] <- scale(df[ , setdiff(names(df), predictors_noscale)])

df_new <- df %>% 
  mutate(attend_diff = attend_p0 - attend_m1, 
         persist_inferred_diff = persist_inferred_p0 - persist_inferred_m1,
         chronic_absentee_diff = chronic_absentee_p0 - chronic_absentee_m1,
         dropout_inferred_diff = dropout_inferred_p0 - dropout_inferred_m1,
         transferred_out_diff = transferred_out_p0 - transferred_out_m1,
         readng_lan_diff = readng_lan_p0 - readng_lan_m1, 
         frl_diff = frl_now - frl_2yr, 
         lep_diff = lep_now - lep_2yr,
         migrant_diff = migrant_now - migrant_2yr,
         homeless_diff = homeless_now - homeless_2yr,
         specialed_diff = specialed_now - specialed_2yr)


df_new_scaled <- df_new 
df_new_scaled[ , setdiff(names(df_new), predictors_noscale)] <- scale(df_new[ , setdiff(names(df_new), predictors_noscale)])


mod_new <- lmer(readng_scr_p0 ~ attend_diff + poly(glmath_scr_m1, 3)
                      + poly(readng_scr_m1,3) + gender + raceth + frl_now 
                      + specialed_now + enrfay_school + transferred_out_diff 
                      + chronic_absentee_diff + readng_lan_diff +  persist_inferred_diff 
                      + homeless_now + migrant_now + lep_now
                      + (1 | schoolid_nces_enroll_p0), data = df_new)

AIC(mod_new) # 1314312
BIC(mod_new) # 1314523
mean(residuals(mod_new)^2) # 8528.561

mod_new_2 <- lmer(readng_scr_p0 ~ attend_diff + poly(glmath_scr_m1, 3)
                + poly(readng_scr_m1,3) + gender + raceth + frl_diff 
                + specialed_diff + enrfay_school + transferred_out_diff 
                + homeless_diff + migrant_diff + lep_diff
                + chronic_absentee_diff + readng_lan_diff +  persist_inferred_diff + homeless_diff
                + (1 | schoolid_nces_enroll_p0), data = df_new)

AIC(mod_new_2) # 1315193
BIC(mod_new_2) # 1315404
mean(residuals(mod_new_2)^2) # 8597.981

mod_new_3 <- lmer(readng_scr_p0 ~ attend_diff + attend_m1 + poly(glmath_scr_m1, 3)
                  + poly(readng_scr_m1,3) + gender + raceth + frl_2yr + frl_diff 
                  + specialed_2yr + transferred_out_m1
                  + specialed_diff + enrfay_school + transferred_out_diff 
                  + homeless_diff + migrant_diff + lep_diff
                  + homeless_2yr + migrant_2yr + lep_2yr
                  + chronic_absentee_m1 + readng_lan_m1 +  persist_inferred_m1 + homeless_2yr
                  + chronic_absentee_diff + readng_lan_diff +  persist_inferred_diff + homeless_diff
                  + (1 | schoolid_nces_enroll_p0), data = df_new)

AIC(mod_new_3) # 1313518
BIC(mod_new_3) # 1313816
mean(residuals(mod_new_3)^2) # 8473.742

# mod_new_4 <- lmer(readng_scr_p0 ~ attend_diff + attend_m1 + poly(glmath_scr_m1, 3)
#                   + poly(readng_scr_m1,3) + gender + raceth + frl_2yr + frl_diff 
#                   + specialed_2yr + transferred_out_m1
#                   + specialed_diff + enrfay_school + transferred_out_diff 
#                   + homeless_diff + migrant_diff + lep_diff
#                   + homeless_2yr + migrant_2yr + lep_2yr
#                   + chronic_absentee_m1 + readng_lan_m1 +  persist_inferred_m1 + homeless_2yr
#                   + chronic_absentee_diff + readng_lan_diff +  persist_inferred_diff + homeless_diff
#                   + (1 | schoolid_nces_enroll_p0), data = df_new_scaled)
# 
# AIC(mod_new_4) # 1313518
# BIC(mod_new_4) # 1313816
# mean(residuals(mod_new_3)^2) # 8473.742
