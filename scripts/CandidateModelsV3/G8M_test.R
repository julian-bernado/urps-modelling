set.seed(489)
library(readr)
library(dplyr)
library(lme4)
library(purrr)
data <- read_csv("/home/rstudio/TEA_2019.csv")
unique_schools <- unique(data$schoolid_nces_enroll_p0)
schools_sampled <- sample(unique_schools, size = round(0.3 * length(unique_schools)))

df <- data %>% 
  filter(schoolid_nces_enroll_p0 %in% schools_sampled) %>% 
  # Remove variables with only NA's
  select(where(~ !all(is.na(.)))) %>% 
  # Remove irrelevant readng scores.
  select(-c('readng_ver_p0', 
            'readng_lan_p0', 
            'readng_scr_p0', 
            'readng_alt_scr_m1', 'readng_alt_scr_p0',
            'glmath_alt_scr_m1', 'glmath_alt_scr_p0')) %>% 
  select(-c('replacement_id', 'acadyear')) %>% 
  # Remove those didn't have exam scores
  filter(!is.na(glmath_scr_p0), gradelevel == 8, !is.na(glmath_scr_m1)) %>% 
  mutate(age_int = round(age))

df0 <- df %>% 
  filter(glmath_scr_p0 == 1043)

# Now we check whether there's underlying pattern
# First check school id
length(unique(df0$schoolid_nces_enroll_p0))
length(unique(df$schoolid_nces_enroll_p0))

df %>% 
  group_by(schoolid_nces_enroll_p0) %>% 
  summarize(n_0 = sum(glmath_scr_p0 == 1043), 
            n = n())%>% 
  arrange(desc(n_0))
# We cannot conclude that this is related to school. 
# What about district?     no
# Gender  no
vars <- names(df)[sapply(df, function(x) length(unique(x)) < 10)]
get_summary <- function(var){
  df %>% 
    group_by(!!sym(var)) %>% 
    summarize(n_0 = sum(glmath_scr_p0 == 1043), 
              n = n(), 
              prop = n_0 / n)%>% 
    arrange(desc(n_0))
}

summary_list <- map(vars, get_summary)
summary_list

vars1 <- names(df)[sapply(df, function(x) !(length(unique(x)) < 10))]

summary_list <- map(vars1, get_summary)
summary_list


# Nothing special was found. 
# But this may be a good thing

