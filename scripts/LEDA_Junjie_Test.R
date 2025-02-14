library(readr)
library(dplyr)
data <- read_csv("../../tea/bernado/TEA_2019.csv")

#nrow(data)

# Have a look at the `class` of the variables
df_class <- data.frame(classes = sapply(data, class))

print(df_class)