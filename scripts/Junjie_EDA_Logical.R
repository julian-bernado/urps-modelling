library(readr)
library(dplyr)
data <- read_csv("../../tea/bernado/TEA_2019.csv", n_max = 1000)

## Data
##We check data for each predictor variable. 

summary_df <- data.frame(
  missing_values = colSums(is.na(data)),
  num_uni_value = sapply(data, function(x) length(unique(x))),
)
summary_df <- summary_df |> 
  arrange(missing_values)
print(summary_df)

