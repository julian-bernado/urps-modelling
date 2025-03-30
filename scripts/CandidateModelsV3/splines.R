# Investigate splines for G4R
# Base model
library(splines)

sp <- list()
sp[['mod1']] <- lmer(scale(readng_scr_p0) ~ poly(scale(readng_scr_m1),3)
                     + gender + raceth + enrfay_school
                     + attend_m1 + attend_p0
                     + lep_now + transferred_out_p0 + chronic_absentee_p0 + readng_lan_p0 +  persist_inferred_p0 
                     + frl_now + specialed_now  + migrant_now + homeless_now + specialed_now 
                     + (1 | schoolid_nces_enroll_p0), data = df)


for (i in 2:10) {
  sp[[paste0("mod", i)]] <- lmer(scale(readng_scr_p0) ~ ns(scale(readng_scr_m1),i)
                                 + gender + raceth + enrfay_school
                                 + ns(scale(attend_m1),i) + ns(scale(attend_p0),i)
                                 + lep_now + transferred_out_p0 + chronic_absentee_p0 + readng_lan_p0 +  persist_inferred_p0 
                                 + frl_now + specialed_now  + migrant_now + homeless_now + specialed_now 
                                 + (1 | schoolid_nces_enroll_p0), data = df)
  
}




sp_Sum <- list()

for(i in seq_along(sp)) {
  sp_Sum[[i]] <- list(
    model = sp[[i]],
    AIC = AIC(sp[[i]]),
    BIC = BIC(sp[[i]]),
    MSE = mean(residuals(sp[[i]])^2)
  )
}
names(sp_Sum) <- names(sp)

aic_values <- sapply(sp_Sum, function(x) x$AIC)
aic_values

bic_values <- sapply(sp_Sum, function(x) x$BIC)
bic_values

mse_values <- sapply(sp_Sum, function(x) x$MSE)
mse_values
