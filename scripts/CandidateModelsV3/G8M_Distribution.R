df_8M <- data[data$gradelevel == 8 & !is.na(data$glmath_scr_p0) & data$glmath_scr_p0 < 1400,]


vars <- names(df_8M)[sapply(df_8M, function(x) length(unique(x)) < 10)]

get_summary <- function(var){
  df_8M %>% 
    group_by(!!sym(var)) %>% 
    summarize(mean(glmath_scr_p0), 
              median(glmath_scr_p0),
              count = n(),
              proportion = n()/nrow(df_8M))
}

summary_list <- map(vars, get_summary)
summary_list





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
  # Remove irrelevant readng scores.
  select(-c('readng_ver_p0', 
            'readng_lan_p0', 
            'readng_scr_p0', 
            'readng_alt_scr_m1', 'readng_alt_scr_p0',
            'glmath_alt_scr_m1', 'glmath_alt_scr_p0')) %>% 
  select(-c('replacement_id', 'acadyear')) %>% 
  # Remove those didn't have exam scores
  filter(!is.na(glmath_scr_p0), !(glmath_scr_p0 == 1043), gradelevel == 8, !is.na(glmath_scr_m1)) %>% 
  mutate(age_int = round(age))
################################################################################
G8M <- list()
G8M[["mod0"]] <- lmer(glmath_scr_p0 ~ poly(glmath_scr_m1,3)
                      + gender + raceth + enrfay_school + age
                      + attend_m1 + attend_p0
                      + lep_now + transferred_out_p0 + chronic_absentee_p0 + glmath_lan_p0 +  persist_inferred_p0 
                      + frl_now + specialed_now  + migrant_now + homeless_now + specialed_now 
                      + (1 | schoolid_nces_enroll_p0), data = df)
G8M[["mod1"]] <- lmer(glmath_scr_p0 ~ poly(glmath_scr_m1,3)
                      + gender + raceth + enrfay_school + age
                      + attend_m1 + attend_p0
                      + lep_now + transferred_out_p0 + chronic_absentee_p0 + glmath_lan_p0 +  persist_inferred_p0 
                      + frl_now + specialed_now  + migrant_now + homeless_now + specialed_now 
                      + (1 | schoolid_nces_enroll_p0), data = df)




G8M_Sum <- list()

for(i in seq_along(G8M)) {
  G8M_Sum[[i]] <- list(
    model = G8M[[i]],
    AIC = AIC(G8M[[i]]),
    BIC = BIC(G8M[[i]]),
    MSE = mean(residuals(G8M[[i]])^2)
  )
}
names(G8M_Sum) <- names(G8M)

aic_values <- sapply(G8M_Sum, function(x) x$AIC)
aic_values

bic_values <- sapply(G8M_Sum, function(x) x$BIC)
bic_values

mse_values <- sapply(G8M_Sum, function(x) x$MSE)
mse_values




model_data <- G8M[["mod0"]]@frame
scaled_predictions <- predict(G8M[["mod1"]])
avg<- mean(model_data$glmath_scr_p0, na.rm = TRUE)
sd <- sd(model_data$glmath_scr_p0, na.rm = TRUE)

original_predictions <- (scaled_predictions * sd) + mean

mse <- mean((model_data$glmath_scr_p0 - original_predictions)^2)

print(mse)




################################################################################






data <- read_csv("/home/rstudio/TEA_2019.csv")
unique_schools <- unique(data$schoolid_nces_enroll_p0)
schools_sampled <- sample(unique_schools, size = round(0.3 * length(unique_schools)))
data_sampled <- data %>% 
  filter(schoolid_nces_enroll_p0 %in% schools_sampled)


df_math <- data_sampled %>% 
  select(gradelevel, glmath_scr_p0, glmath_lan_p0) %>% 
  na.omit()

hist_math_g8_lan5 <- df_math %>%
  filter(gradelevel == 8, glmath_lan_p0 == 5, glmath_scr_p0 < 1100) %>%
  ggplot(aes(x = glmath_scr_p0)) +
  geom_histogram(bins = 100) +
  labs(
    title = "Grade 8 Math Scores (glmath_lan_p0 = 5)",
    x = "Score",
    y = "Count"
  ) +
  theme_minimal()

  
df1 <-   data[data$gradelevel == 8 & !is.na(data$glmath_scr_p0),]
  
  # Find mode of math_grade
  mode_val <- names(sort(table(df1$glmath_scr_p0), decreasing = TRUE))[1]
  mode_val <- as.numeric(mode_val)  # optional, if it's numeric
  print(mode_val)
  

  
lmer(scale(readng_scr_p0) ~ poly(scale(readng_scr_m1),3)
                        + gender + raceth + enrfay_school + age
                        + attend_m1 + attend_p0
                        + lep_now + transferred_out_p0 + chronic_absentee_p0 + readng_lan_p0 +  persist_inferred_p0 
                        + frl_now + specialed_now  + migrant_now + homeless_now + specialed_now 
                        + (1 | schoolid_nces_enroll_p0), data = df)
  
nrow(data[data$glmath_scr_p0 == 1043 & !is.na(data$glmath_scr_p0) & data$gradelevel ==8,])
# [1] 62472