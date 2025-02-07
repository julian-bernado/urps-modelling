# This file should take in the 
set.seed(2025)
library(readr)
library(dplyr)
library(haven)

# These are the variables we're going to extract

# First, let's select the variables that don't have a lagged version or older versions
covariates <- c("replacement_id",
                "acadyear",
                "enrfay_school",
                "enrfay_district",
                "enrfay_state",
                "gradelevel",
                "age",
                "gender",
                "raceth")

# Now, the variables that have a _now, _2yr, _3yr, _5yr, _ever, _elem, _midd, _high suffix
history_covariate_stems <- c("frl",
                        "lep",
                        "rfep",
                        "migrant",
                        "homeless",
                        "specialed")

history_covariates <- c()

for(suffix in c("_now", "_2yr", "_3yr", "_5yr", "_ever", "_elem", "_midd", "_high")){
  history_covariates <- c(history_covariates, paste0(history_covariate_stems, suffix))
}

# Now the variables that have a _p0 or _m1 suffix
lagged_covariate_stems <- c("attend",
                       "gpacum",
                       "dropout_inferred",
                       "withdrawal_date",
                       "persist_inferred",
                       "transferred_out",
                       "chronic_absentee",
                       "glmath_scr",
                       "glmath_ver",
                       "glmath_scr",
                       "glmath_lan",
                       "glmath_alt_scr",
                       "readng_scr",
                       "readng_ver",
                       "readng_scr",
                       "readng_lan",
                       "readng_alt_scr",
                       "districtid_nces_enroll",
                       "schoolid_nces_enroll")

lagged_covariates <- c()

for(suffix in c("_p0", "_m1")){
  lagged_covariates <- c(lagged_covariates, paste0(lagged_covariate_stems, suffix))
}

columns_to_pull <- c(covariates, history_covariates, lagged_covariates)

# Now we read in the data
wide_data <- read_dta("../../tea/data/current/TX2019_DRV_UMICH.dta", col_select = all_of(columns_to_pull)) %>%
  filter(gradelevel %in% 3:8)

# Order the data by school and student
wide_data <- wide_data %>%
  arrange(schoolid_nces_enroll_p0, replacement_id)

# Write file
write_csv(wide_data, "TEA_2019.csv")
