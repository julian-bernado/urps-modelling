source("~/urps-modelling/scripts/Z_helpers.R")

set.seed(489)
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

candidate_vars_1 <- c("frl_ever",
                      "lep_ever",
                      "attend_p0_d1",
                      "specialed_ever",
                      "transferred_out_p0",
                      "enrfay_school",
                      "chronic_absentee_p0",
                      "homeless_ever",
                      "persist_inferred_p0",
                      "migrant_ever")

fixed_vars_1 <- c("readng_scr_m1",
                  "gender",
                  "raceth")
c_mod_list <- create_candidate_mod_list()
c_mod_list <- create_candidate_mod(candidate_vars_1, fixed_vars_1, "r", 5)



candidate_vars_2 <- c("persist_inferred_m1",
                      "transferred_out_m1",
                      "chronic_absentee_m1",
                      "readng_lan_m1", 
                      "attend_m1_d1")

fixed_vars_2 <- c("readng_scr_m1",
                  "gender",
                  "raceth",
                  "frl_ever",
                  "lep_ever",
                  "attend_p0_d1",
                  "specialed_ever",
                  "transferred_out_p0",
                  "enrfay_school",
                  "chronic_absentee_p0")

c_mod_list_2 <- create_candidate_mod_list()
c_mod_list_2 <- create_candidate_mod(candidate_vars_2, fixed_vars_2, "r", 5, c_mod_list_2)
model_selection(5, "r", c_mod_list_2)
