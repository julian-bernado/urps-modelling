source("~/urps-modelling/scripts/Z_helpers.R")

library(readr)
library(dplyr)
library(lme4)
library(purrr)
library(ggplot2)
library(stringr)
library(splines)
data <- read_csv("/home/rstudio/TEA_2019.csv")

df <- create_dataset(data)
dfs <- clean_dataset(df)



