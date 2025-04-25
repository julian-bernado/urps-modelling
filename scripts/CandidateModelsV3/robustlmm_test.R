library(robustlmm)
data("Penicillin")

fm <- lmer(diameter ~ (1 | plate) + (1 | sample), Penicillin)
rfm <- rlmer(diameter ~ (1 | plate) + (1 | sample), Penicillin)
summary(rfm)
