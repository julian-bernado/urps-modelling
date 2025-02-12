data <- read.csv("../../tea/bernado/TEA_2019.csv")
data <- read.csv("/Users/junjiezeng/Desktop/Fall2024/STATS 500/Datasets/Titanic copy.csv")
library(dplyr)

## Data
##We check data for each predictor variable. 

summary_df <- data.frame(
  missing_values = colSums(is.na(data)),
  num_uni_value = sapply(data, function(x) length(unique(x))),
  uni_value = paste(lapply(data, unique), sep =",")
)
summary_df <- summary_df |> 
  arrange(missing_values)
print(summary_df)

