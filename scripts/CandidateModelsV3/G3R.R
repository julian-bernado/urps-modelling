################################################################################
# Data
set.seed(489)
library(readr)
library(dplyr)
library(lme4)
library(purrr)
data <- read_csv("/home/rstudio/TEA_2019.csv")
unique_schools <- unique(data$schoolid_nces_enroll_p0)
schools_sampled <- sample(unique_schools, size = round(0.3 * length(unique_schools)))
df <- data %>% 
  filter(schoolid_nces_enroll_p0 %in% schools_sampled) %>% 
  # Remove variables with only NA's
  select(where(~ !all(is.na(.)))) %>% 
  # Remove irrelevant math scores.
  select(-c('glmath_ver_m1', 'glmath_ver_p0', 
            'glmath_lan_m1', 'glmath_lan_p0', 
            'glmath_scr_m1', 'glmath_scr_p0', 
            'glmath_alt_scr_m1', 'glmath_alt_scr_p0',
            'readng_alt_scr_m1', 'readng_alt_scr_p0',
            'withdrawal_date_p0')) %>% 
  # Remove those ends by "_midd"
  select(-ends_with("_midd")) %>% 
  select(-c('readng_ver_m1', 'readng_lan_m1', 'readng_scr_m1', 'replacement_id', 
            'acadyear','dropout_inferred_m1', 'persist_inferred_m1')) %>% 
  # Remove those didn't have exam scores
  filter(!is.na(readng_scr_p0), gradelevel == 3) %>% 
  mutate(age_int = round(age),
         attend_p0_d1 = round(attend_p0, 1))
################################################################################
# We proceed by having a full model and do backward elimination. 

G3_R <- list() 
G3_R[["mod0"]] <- lmer(readng_scr_p0 ~ 1 + (1 | schoolid_nces_enroll_p0), data = df)

# Full Model of our choice
G3_R[["mod1"]] <- lmer(readng_scr_p0 ~ 
                                enrfay_school + enrfay_district + enrfay_state
                              + age_int + gender + raceth + frl_now + lep_now
                              + migrant_now + homeless_now + specialed_now 
                              + attend_p0_d1 + dropout_inferred_p0 + transferred_out_p0
                              + chronic_absentee_p0 + readng_lan_p0
                              + (1 | schoolid_nces_enroll_p0), data = df)

# Remove `enrfay_district`
G3_R[["mod2"]] <- lmer(readng_scr_p0 ~ 
                         enrfay_school + enrfay_state
                       + age_int + gender + raceth + frl_now + lep_now
                       + migrant_now + homeless_now + specialed_now 
                       + attend_p0_d1 + dropout_inferred_p0 + transferred_out_p0
                       + chronic_absentee_p0 + readng_lan_p0
                       + (1 | schoolid_nces_enroll_p0), data = df)

# Remove `dropout_inferred_p0`, `enrfay_district`
G3_R[["mod3"]] <- lmer(readng_scr_p0 ~ 
                         enrfay_school + enrfay_state
                       + age_int + gender + raceth + frl_now + lep_now
                       + migrant_now + homeless_now + specialed_now 
                       + attend_p0_d1 + transferred_out_p0
                       + chronic_absentee_p0 + readng_lan_p0
                       + (1 | schoolid_nces_enroll_p0), data = df)

# Remove `readng_lan_p0`, `dropout_inferred_p0`, `enrfay_district`
G3_R[["mod4"]] <- lmer(readng_scr_p0 ~ 
                         enrfay_school + enrfay_state
                       + age_int + gender + raceth + frl_now + lep_now
                       + migrant_now + homeless_now + specialed_now 
                       + attend_p0_d1 + transferred_out_p0
                       + chronic_absentee_p0
                       + (1 | schoolid_nces_enroll_p0), data = df)

# Remove `enrfay_state`, `readng_lan_p0`, `dropout_inferred_p0`, `enrfay_district`
G3_R[["mod5"]] <- lmer(readng_scr_p0 ~ 
                         enrfay_school
                       + age_int + gender + raceth + frl_now + lep_now
                       + migrant_now + homeless_now + specialed_now 
                       + attend_p0_d1 + transferred_out_p0
                       + chronic_absentee_p0
                       + (1 | schoolid_nces_enroll_p0), data = df)

# Remove `homeless_now`, `enrfay_state`, `readng_lan_p0`, `dropout_inferred_p0`, `enrfay_district`
G3_R[["mod6"]] <- lmer(readng_scr_p0 ~ 
                         enrfay_school
                       + age_int + gender + raceth + frl_now + lep_now
                       + migrant_now + specialed_now 
                       + attend_p0_d1 + transferred_out_p0
                       + chronic_absentee_p0
                       + (1 | schoolid_nces_enroll_p0), data = df)

# We see that the above models have increasing AIC/BIC, which does not seem good
# We now try to start from the best model we had last time

# Best model from last time
G3_R[["mod7"]] <- lmer(readng_scr_p0 ~ 
                         gender + raceth + frl_now + specialed_now + enrfay_school 
                       + transferred_out_p0 + chronic_absentee_m1 + readng_lan_p0 + 
                         homeless_now + migrant_now + persist_inferred_p0 +
                         (1 | schoolid_nces_enroll_p0), data = df)

# Add `attend_p0_d1`
G3_R[["mod8"]] <- lmer(readng_scr_p0 ~ 
                         gender + raceth + frl_now + specialed_now + enrfay_school 
                       + transferred_out_p0 + chronic_absentee_m1 + readng_lan_p0 + 
                         homeless_now + migrant_now + persist_inferred_p0 + attend_p0_d1 +
                         (1 | schoolid_nces_enroll_p0), data = df)

# Remove `persist_inferred_p0`
G3_R[["mod9"]] <- lmer(readng_scr_p0 ~ 
                         gender + raceth + frl_now + specialed_now + enrfay_school 
                       + transferred_out_p0 + chronic_absentee_m1 + readng_lan_p0 + 
                         homeless_now + migrant_now  + attend_p0_d1 +
                         (1 | schoolid_nces_enroll_p0), data = df)


#################################################################################
G3_R_Sum <- list()

for(i in seq_along(G3_R)) {
  G3_R_Sum[[i]] <- list(
    model = G3_R[[i]],
    AIC = AIC(G3_R[[i]]),
    BIC = BIC(G3_R[[i]]),
    MSE = mean(residuals(G3_R[[i]])^2)
  )
}
names(G3_R_Sum) <- names(G3_R)

aic_values <- sapply(G3_R_Sum, function(x) x$AIC)
aic_values

bic_values <- sapply(G3_R_Sum, function(x) x$BIC)
bic_values

mse_values <- sapply(G3_R_Sum, function(x) x$MSE)
mse_values
#################################################################################


# Get fitted values and residuals
fitted_values <- fitted(G3_R[["mod9"]])  # replace fit_model with your model name
residuals <- residuals(G3_R[["mod9"]])

# Plot residuals vs fitted values
par(mar = c(4, 4, 2, 2))
plot(fitted_values, residuals, main="Residuals vs Fitted", xlab="Fitted values", ylab="Residuals")
abline(h = 0, col = "red")

# Histogram of residuals
hist(residuals, main="Histogram of Residuals", xlab="Residuals")









