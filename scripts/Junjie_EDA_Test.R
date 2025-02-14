library(readr)
library(dplyr)
data <- read_csv("../../tea/bernado/TEA_2019.csv")

#nrow(data)

# Have a look at the `class` of the variables
df1 <- data.frame(classes = sapply(data, class))

#print(df1)

df1 <- df1 |> 
  mutate(num_uni = sapply(data, function(x) 
    {
    count <- length(unique(x))
    if (count > 1000) -1 else count
    })
  )

# number of missing values 
df1 <- df1 |> 
  mutate(num_missing = colSums(is.na(data)))



# Adjusted number of unique values
# num_uni_value - 1 if missing value = TRUE 
# This is for an easier analysis on unique values
df1 <- df1 |> 
  mutate(num_uni_adj = ifelse(num_uni > 0 & num_missing > 0, 
                              num_uni - 1, 
                              num_uni))

print(df1)

#enrfay_school 7 should be 2
#enrfay_district 7
#enrfay_state 7
#gradelevel 6
#raceth 6 
uni_list <- c("enrfay_school", 
              "enrfay_district", 
              "enrfay_state", 
              "gradelevel", 
              "raceth")

uni_values <- lapply(data[, uni_list], unique)
print(uni_values)

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
############
### TO-DO
### Five-num summary of above variables
summary_stats <- data %>%
  summarise(across(all_of(summary_vars), 
                   ~ quantile(., probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE))) %>%
  t() %>%
  as.data.frame() %>%
  setNames(c("Min", "Q1", "Median", "Q3", "Max"))

print(summary_stats)


#outlier_counts <- data %>%
#  summarise(across(where(is.numeric), ~ sum(. < (quantile(., 0.25, na.rm = TRUE) - 1.5 * IQR(.)) | 
#                                              . > (quantile(., 0.75, na.rm = TRUE) + 1.5 * IQR(.)), na.rm = TRUE)))
#print(outlier_counts)
