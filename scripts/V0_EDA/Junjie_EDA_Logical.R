# For reproducibility
set.seed(214) 
library(readr)
library(dplyr)


# Data
data <- read.csv("../../tea/bernado/TEA_2019.csv")
### Some facts about the data
### n_col = 91, n_row = 2506956
### gradelevel : 3, 4, 5, 6, 7, 8
n_col <- ncol(data)
n_row <- nrow(data)
cat("Number of variables: ", n_col, "\n")
cat("Number of observations: ", n_row, "\n")
uni_list <- c("enrfay_school", 
              "enrfay_district", 
              "enrfay_state", 
              "gradelevel", 
              "raceth")
uni_values <- lapply(data[, uni_list], unique)
print(uni_values)


###############################################################################


# Have a look at the `class` of the variables
df1 <- data.frame(classes = sapply(data, class))

# number of unique values for each variable
df1 <- df1 |> 
  mutate(num_uni = sapply(data, function(x) 
  {
    count <- length(unique(x))
    if (count > 1000) -1 else count
  })
  )

# number of missing values for each variable
df1 <- df1 |> 
  mutate(num_missing = colSums(is.na(data)))


# Adjusted number of unique values
# num_uni_value - 1 if missing value = TRUE 
# This is for an easier analysis on unique values
df1 <- df1 |> 
  mutate(num_uni_adj = ifelse(num_uni > 0 & num_missing > 0, 
                              num_uni - 1, 
                              num_uni))
print("Dataframe for unique and missing values: ")
print(df1)


###############################################################################

### df2
# Some variables have only NAs, which are not needed for modeling. 
# So we remove those variables from `df1` to get `df2`
df2 <- df1 |> 
  filter(num_missing != 2506956)

n_vars_df2 = nrow(df2) 
print("Variables with all missing values removed: ")
print(df2)
cat("Number of variables left: ", n_vars_df2, "\n")
###############################################################################

# 5 num summary
# glmath_scr_m1, glmath_scr_p0, glmath_alt_scr_m1, glmath_alt_scr_p0
# readng_scr_m1, readng_scr_p0, readng_alt_scr_m1, readng_alt_scr_p0
summary_vars <- c("glmath_scr_m1", 
                  "glmath_scr_p0", 
                  "glmath_alt_scr_m1",
                  "glmath_alt_scr_p0",
                  "readng_scr_m1",
                  "readng_scr_p0",
                  "readng_alt_scr_m1",
                  "readng_alt_scr_p0")
summary_stats <- data %>%
  summarise(across(all_of(summary_vars), 
                   ~ quantile(., probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE))) %>%
  t() %>%
  as.data.frame() %>%
  setNames(c("Min", "Q1", "Median", "Q3", "Max"))

print("Five number summary statistics dataframe: ")
print(summary_stats)


###############################################################################
### TO-DO
### Find outliers? ? ?

#outlier_counts <- data %>%
#  summarise(across(where(is.numeric), ~ sum(. < (quantile(., 0.25, na.rm = TRUE) - 1.5 * IQR(.)) | 
#                                              . > (quantile(., 0.75, na.rm = TRUE) + 1.5 * IQR(.)), na.rm = TRUE)))
#print(outlier_counts)





