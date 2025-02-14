# Searching for Extraneous Values 

library(dplyr)
library(readr)
library(tidyverse)

# add nrow argument 
# read_csv readr (tidy) 
df <- read_csv("../../tea/bernado/TEA_2019.csv", n_max = 100)

# 5-number summary 
df %>% 
  select_if(is.numeric) %>% 
  sapply(MARGIN = 2, summary) 

