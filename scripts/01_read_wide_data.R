# 
set.seed(439)
library(readr)
library(dplyr)
library(haven)
library(R.utils)
source("helpers.R")

# These are the variables we're going to extract
covariates <- c("studentid_state",
                "schoolid_nces_enroll",
                "gradelevel",
                "acadyear",
                "glmath_scr",
                "glmath_ver",
                "readng_scr",
                "readng_ver",
                "gender",
                "lep",
                "specialed",
                "raceth")


# Now we read in the data
wide_data <- read_dta("../../tea/data/current/TX2022_LNG_UMICH.dta", col_select = all_of(columns_to_pull), n_max = num_rows) 

# Once we've read in the data, we want to subset the data such that records should exist for this year, those who are in the specific grade, and we'll remove the grade level column for redundancy
wide_data <- wide_data %>%
  filter(get(paste("acadyear", year_of_interest, sep = "_")) == 1) %>%
  filter(get(paste("gradelevel", year_of_interest, sep = "_")) == paste0("0", grade_of_interest)) %>% 
  select(-all_of(c(paste("gradelevel", year_of_interest, sep = "_")))) %>%
  select(-all_of(c(paste("acadyear", year_of_interest, sep = "_"))))

# Order the data by school and student
wide_data <- wide_data %>%
  arrange(schoolid_nces_enroll, studentid_state)

# We add in the average demographics for a school as a student-level covariate (does not include school, student id, or test scores)
# Additionally, we subtract the average covariate value within a school from each observation
wide_data <- add_col_avgs(wide_data, covars_to_avg, grouping = "schoolid_nces_enroll")

# Output a .CSV corresponding to the (year, grade, subject) combination

# Prepare output file name and directory
output_directory <- "../data/"
file_prefix <- paste(year_of_interest, grade_of_interest, subject, sep = "_")
base_name <- "regression_ready.csv"
output_file_name <- paste(file_prefix, base_name, sep = "_")

# Write file
write_csv(subject_data, paste0(output_directory, output_file_name))
