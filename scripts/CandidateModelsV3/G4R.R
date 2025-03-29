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

################################################################################

G4R <- list()

G4R[['mod0']] <- lmer(readng_scr_p0 ~ 1 + (1 | schoolid_nces_enroll_p0), data = df)

# Start from the best model last time 
G4R[["mod1"]] <- lmer(readng_scr_p0 ~ 
                                 readng_scr_m1 + gender + raceth + frl_now 
                               + specialed_now + enrfay_school + transferred_out_p0 
                               + chronic_absentee_p0 + readng_lan_p0 +  persist_inferred_p0 
                               + (1 | schoolid_nces_enroll_p0), data = df)

# Add `glmath_scr_m1`
G4R[["mod2"]] <- lmer(readng_scr_p0 ~ glmath_scr_m1
                      + readng_scr_m1 + gender + raceth + frl_now 
                      + specialed_now + enrfay_school + transferred_out_p0 
                      + chronic_absentee_p0 + readng_lan_p0 +  persist_inferred_p0 
                      + (1 | schoolid_nces_enroll_p0), data = df)

# Add `attend_p0_d1`
G4R[["mod3"]] <- lmer(readng_scr_p0 ~ attend_p0_d1 + glmath_scr_m1
                      + readng_scr_m1 + gender + raceth + frl_now 
                      + specialed_now + enrfay_school + transferred_out_p0 
                      + chronic_absentee_p0 + readng_lan_p0 +  persist_inferred_p0 
                      + (1 | schoolid_nces_enroll_p0), data = df)

# Add `attend_m1_d1`
G4R[["mod4"]] <- lmer(readng_scr_p0 ~ attend_m1_d1 + attend_p0_d1 + glmath_scr_m1
                      + readng_scr_m1 + gender + raceth + frl_now 
                      + specialed_now + enrfay_school + transferred_out_p0 
                      + chronic_absentee_p0 + readng_lan_p0 +  persist_inferred_p0 
                      + (1 | schoolid_nces_enroll_p0), data = df)

# Add polynomial terms for `readng_scr_m1`
# We do see a significant decrease in AIC/BIC/MSE
G4R[["mod5"]] <- lmer(readng_scr_p0 ~ attend_m1_d1 + attend_p0_d1 + glmath_scr_m1
                      + poly(readng_scr_m1,3) + gender + raceth + frl_now 
                      + specialed_now + enrfay_school + transferred_out_p0 
                      + chronic_absentee_p0 + readng_lan_p0 +  persist_inferred_p0 
                      + (1 | schoolid_nces_enroll_p0), data = df)

# Add polynomial terms for `glmath_scr_m1`
# Not very helpful
G4R[["mod6"]] <- lmer(readng_scr_p0 ~ attend_m1_d1 + attend_p0_d1 + poly(glmath_scr_m1, 3)
                      + poly(readng_scr_m1,3) + gender + raceth + frl_now 
                      + specialed_now + enrfay_school + transferred_out_p0 
                      + chronic_absentee_p0 + readng_lan_p0 +  persist_inferred_p0 
                      + (1 | schoolid_nces_enroll_p0), data = df)
# Scaled model
G4R[["mod7"]] <- lmer(scale(readng_scr_p0) ~ poly(scale(readng_scr_m1),3)
                      + gender + raceth + enrfay_school
                      + scale(attend_m1_d1) + scale(attend_p0_d1)
                      + lep_now + transferred_out_p0 + chronic_absentee_p0 + readng_lan_p0 +  persist_inferred_p0 
                      + frl_now + specialed_now  + migrant_now + homeless_now + specialed_now 
                      + (1 | schoolid_nces_enroll_p0), data = df)

# Scaled model
G4R[["mod8"]] <- lmer(scale(readng_scr_p0) ~ poly(scale(readng_scr_m1),3)
                      + gender + raceth + enrfay_school + age
                      + attend_m1 + attend_p0
                      + lep_now + transferred_out_p0 + chronic_absentee_p0 + readng_lan_p0 +  persist_inferred_p0 
                      + frl_now + specialed_now  + migrant_now + homeless_now + specialed_now 
                      + (1 | schoolid_nces_enroll_p0), data = df)


#################################################################################

G4R_Sum <- list()

for(i in seq_along(G4R)) {
  G4R_Sum[[i]] <- list(
    model = G4R[[i]],
    AIC = AIC(G4R[[i]]),
    BIC = BIC(G4R[[i]]),
    MSE = mean(residuals(G4R[[i]])^2)
  )
}
names(G4R_Sum) <- names(G4R)

aic_values <- sapply(G4R_Sum, function(x) x$AIC)
aic_values

bic_values <- sapply(G4R_Sum, function(x) x$BIC)
bic_values

mse_values <- sapply(G4R_Sum, function(x) x$MSE)
mse_values

#################################################################################
# sampled_df <- df[sample(nrow(df), 10000), ]
# 
# ggplot(sampled_df, aes(x = glmath_scr_m1, y = readng_scr_p0)) +
#   geom_hex(bins = 50) +                       
#   geom_smooth(method = "loess", se = FALSE, color = "red") +
#   labs(title = "Current vs Past score (smoothed)")

model_data <- G4R[["mod6"]]@frame
scaled_predictions <- predict(G4R[["mod7"]])


mean<- mean(model_data$readng_scr_p0, na.rm = TRUE)
sd <- sd(model_data$readng_scr_p0, na.rm = TRUE)

original_predictions <- (scaled_predictions * sd) + mean

mse <- mean((model_data$readng_scr_p0 - original_predictions)^2)

print(mse)


