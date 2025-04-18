set.seed(489)
library(readr)
library(dplyr)
library(lme4)
library(purrr)
data <- read_csv("/home/rstudio/TEA_2019.csv")

nrow(data)
nrow(data[data$frl_now == 1,])
nrow(data[data$frl_now == 0,])
nrow(data[is.na(data$frl_now),])

data2 <- read_csv("~/urps-main/TEA_2022.csv")
nrow(data2)
nrow(data2[data2$frl_now == 1,])
nrow(data2[data2$frl_now == 0,])
nrow(data2[is.na(data2$frl_now),])

all_formulas <- readRDS("~/urps-main/formulas/all_formulas.rds")
final_formulas <- readRDS("~/urps-main/formulas/final_formulas.rds")
final_formulas
