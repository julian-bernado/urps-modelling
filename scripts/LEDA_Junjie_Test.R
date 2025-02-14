library(readr)
library(dplyr)
data <- read_csv("../../tea/bernado/TEA_2019.csv")

#nrow(data)

# Have a look at the `class` of the variables
df1 <- data.frame(classes = sapply(data, class))

print(df1)

df1 <- df1 |> 
  mutate(num_uni_value = sapply(data, function(x) 
    {
    count <- length(unique(x))
    if (count > 1000) -1 else count
    })
  )

print(df1)