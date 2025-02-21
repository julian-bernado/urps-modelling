library(dplyr)
library(readr)
library(tidyverse)

df <- read_csv("TEA_2019.csv")

na_df = data.frame(colSums(is.na(df))) %>% 
  mutate(total_na = colSums.is.na.df..) %>% 
  select(total_na)

no_data_columns = c(na_df %>%
                      dplyr::filter(total_na == nrow(df)) %>% 
                      rownames())



# removing columns that are all NA
df_new = df %>% select(-all_of(no_data_columns)) 

# Showing that just because alternate for one test doesn't mean alternate for both
# but VERY few of these students 
df_new %>% 
  filter(readng_ver_m1 != glmath_ver_m1) %>% 
  nrow()

# 1 standard or regular, 2 alternative, 4 other 
df_new %>% 
  dplyr::select(readng_ver_m1) %>% 
  drop_na()

# 2,747 children changed between the years (reading)
df_new %>% 
  dplyr::select(readng_ver_m1, readng_ver_p0) %>% 
  drop_na() %>% 
  filter(readng_ver_m1 != readng_ver_p0) %>% 
  nrow()

# 2, 753 children changed between the years (math)
df_new %>% 
  dplyr::select(glmath_ver_m1, glmath_ver_p0) %>% 
  drop_na() %>% 
  filter(glmath_ver_m1 != glmath_ver_p0) %>% 
  nrow()

# comparable amount of children changed between years 

change_ver_test_df = df_new %>% 
  dplyr::select(readng_ver_m1, readng_ver_p0, glmath_ver_m1, glmath_ver_p0) %>% 
  drop_na() %>% 
  filter(glmath_ver_m1 != glmath_ver_p0, readng_ver_m1 != readng_ver_p0) %>% 
  mutate(direction_change_read = readng_ver_m1 - readng_ver_p0, 
         direction_change_math = glmath_ver_m1 - glmath_ver_p0) 

# indicating that children always changed the same direction, both went normal or both went alternative
change_ver_test_df %>% 
  filter(direction_change_math != direction_change_read)

# 1,244 children changed from alternative to normal 
change_ver_test_df %>% 
  filter(glmath_ver_p0 == 1) %>% 
  nrow()

# 1,491 children changed from normal to alternative
change_ver_test_df %>% 
  filter(glmath_ver_p0 == 2) %>% 
  nrow()

# Looking at math and reading scores 
df_new %>% 
  ggplot(aes(x = glmath_alt_scr_m1, y = glmath_alt_scr_p0)) + 
  geom_point() + 
  geom_smooth(method = "lm")

df_new %>% 
  ggplot(aes(x = glmath_scr_m1, y = glmath_scr_p0)) + 
  geom_point() + 
  geom_smooth(method = "lm")

df_new %>% 
  ggplot(aes(x = readng_alt_scr_m1, y = readng_alt_scr_p0)) + 
  geom_point() + 
  geom_smooth(method = "lm")

df_new %>% 
  ggplot(aes(x = readng_scr_m1, y = readng_scr_p0)) + 
  geom_point() + 
  geom_smooth(method = "lm")

df_new %>% 
  ggplot(aes(x = readng_scr_m1, y = glmath_scr_m1)) + 
  geom_point() + 
  geom_smooth(method = "lm")

df_new %>% 
  ggplot(aes(x = readng_scr_p0, y = glmath_scr_p0)) + 
  geom_point() + 
  geom_smooth(method = "lm")

df_new %>% 
  ggplot(aes(x = readng_alt_scr_m1, y = glmath_alt_scr_m1)) + 
  geom_point() + 
  geom_smooth(method = "lm")

df_new %>% 
  ggplot(aes(x = readng_alt_scr_p0, y = glmath_alt_scr_p0)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# Stratifying by version they took that year 

# Finding correlations

# Scores with attendance percentage
# Regular Test
df_new %>% 
  dplyr::select(attend_m1, glmath_scr_m1, readng_scr_m1) %>% 
  drop_na() %>% 
  cor()

df_new %>% 
  dplyr::select(attend_p0, glmath_scr_p0, readng_scr_p0) %>% 
  drop_na() %>% 
  cor()

# Alternative test (much more correlation between reading and math scores, but maybe because less people)
df_new %>% 
  dplyr::select(attend_m1, glmath_alt_scr_m1, readng_alt_scr_m1) %>% 
  drop_na() %>% 
  cor()

df_new %>% 
  dplyr::select(attend_p0, glmath_alt_scr_p0, readng_alt_scr_p0) %>% 
  drop_na() %>% 
  cor()
# Would be interesting to see, if they changed from alternative to normal how do they perform 
# and vice versa 



