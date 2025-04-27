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

c_mod_list <- create_candidate_mod_list()
colnames(dfs[["df_3_r"]])


candidate_vars <- c("frl_ever", 
                   "lep_ever", 
                   "migrant_ever", 
                   "homeless_ever", 
                   "specialed_ever",
                   "chronic_absentee_p0", 
                   "readng_ver_m1")

fixed_vars <- c("readng_scr_m1",
                "gender",
                "raceth")

start_time <- Sys.time()
create_candidate_mod(candidate_vars, fixed_vars, "r", 4)
end_time <- Sys.time()

print(end_time - start_time)

