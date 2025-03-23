library(ggplot2)
library(readr)
library(dplyr)
library(lme4)
library(purrr)
set.seed(485)
data <- read_csv("/home/rstudio/TEA_2019.csv")
unique_schools <- unique(data$schoolid_nces_enroll_p0)

schools_sampled <- sample(unique_schools, size = round(0.3 * length(unique_schools)))
df <- data %>% 
  filter(schoolid_nces_enroll_p0 %in% schools_sampled)

p1 <- ggplot(df, aes(x = readng_scr_m1, y = readng_scr_p0)) +
  geom_hex(bins = 50) +                       
  geom_smooth(method = "loess", se = FALSE, color = "red") +                           # or your categorical factor
  labs(title = "Current vs Past score (smoothed)")

ggsave(filename="m1VSp0ReadingScores.png", plot = p1, width = 8, height = 6, dpi = 300)

p2 <- ggplot(df, aes(x = glmath_scr_m1, y = glmath_scr_p0)) +
  geom_hex(bins = 50) +                       
  geom_smooth(se = FALSE, color = "red") +                           # or your categorical factor
  labs(title = "Current vs Past score (smoothed)")

ggsave(filename="m1VSp0MathScores.png", plot = p2, width = 8, height = 6, dpi = 300)




### Generated using different data 

df_poly <- df %>% 
  filter(!is.na(readng_scr_m1))

mod_poly <- list() 
for(i in 1:10){
  mod_poly[[i]] <- lmer(readng_scr_p0 ~ poly(readng_scr_m1,i) + 
                        (1 | schoolid_nces_enroll_p0), data = df_poly)
}

mod_poly_sum <- list()

for(i in seq_along(mod_poly)) {
  mod_poly_sum[[i]] <- list(
    model = mod_poly[[i]],
    AIC = AIC(mod_poly[[i]]),
    BIC = BIC(mod_poly[[i]]),
    MSE = mean(residuals(mod_poly[[i]])^2)
  )
}



aic_values <- sapply(mod_poly_sum, function(x) x$AIC)
aic_values
##[1] 1367425 1367340 1360385 1360376 1360090
##[6] 1360077 1360052 1360039 1360029 1360009

bic_values <- sapply(mod_poly_sum, function(x) x$BIC)
bic_values
## [1] 1367464 1367389 1360443 1360443 1360167
## [6] 1360164 1360148 1360145 1360144 1360134

mse_values <- sapply(mod_poly_sum, function(x) x$MSE)
mse_values
##[1] 10150.706 10144.240  9544.970  9544.971
##[5]  9521.458  9521.197  9519.834  9519.523
##[9]  9519.390  9518.536

Both plot and numerical values suggested a polynomial of three is good enough. 