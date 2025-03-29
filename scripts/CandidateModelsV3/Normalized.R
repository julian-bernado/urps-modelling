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
  filter(!is.na(readng_scr_m1), !is.na(glmath_scr_m1))

df_scaled <- df
predictors_noscale <- c('districtid_nces_enroll_m1', 'districtid_nces_enroll_p0', 
                        'schoolid_nces_enroll_m1', 'schoolid_nces_enroll_p0','gradelevel')
# Assuming predictors_noscale is a vector of column names you don't want to scale
df_scaled[ , setdiff(names(df), predictors_noscale)] <- scale(df[ , setdiff(names(df), predictors_noscale)])

df_normalized <- 



# Scaled model
all_scaled <- lmer(readng_scr_p0 ~ attend_m1_d1 + attend_p0_d1 + poly(glmath_scr_m1, 3)
                      + poly(readng_scr_m1,3) + gender + raceth + frl_now 
                      + specialed_now + enrfay_school + transferred_out_p0 
                      + chronic_absentee_p0 + readng_lan_p0 +  persist_inferred_p0 
                      + (1 | schoolid_nces_enroll_p0), data = df_scaled)

AIC(all_scaled) # 209107
BIC(all_scaled) # 209299.3


con_scaled <- lmer(scale(readng_scr_p0) ~ scale(attend_m1_d1) + attend_p0_d1 + poly(scale(glmath_scr_m1), 3)
                   + poly(scale(readng_scr_m1,3)) + gender + raceth + frl_now 
                   + specialed_now + enrfay_school + transferred_out_p0 
                   + chronic_absentee_p0 + readng_lan_p0 +  persist_inferred_p0 
                   + (1 | schoolid_nces_enroll_p0), data = df)

AIC(con_scaled) # 212771.9
BIC(con_scaled) # 212944.9


