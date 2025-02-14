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

###
#enrfay_school 7 should be 2
#enrfay_district
#enrfay_state
#gradelevel 6
#gpacum
#trasferred should 4


# glmath_scr_m1, glmath_scr_p0, glmath_alt_scr_m1, glmath_alt_scr_p0
# readng_scr_m1, readng_scr_p0, readng_alt_scr_m1, readng_alt_scr_p0