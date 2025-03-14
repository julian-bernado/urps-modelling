  # Reproducibility   
  
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
    # Remove irrelevant math scores.
    select(-c('glmath_ver_m1', 'glmath_ver_p0', 
              'glmath_lan_m1', 'glmath_lan_p0', 
              'glmath_scr_m1', 'glmath_scr_p0', 
              'glmath_alt_scr_m1', 'glmath_alt_scr_p0',
              'readng_alt_scr_m1', 'readng_alt_scr_p0',
              'withdrawal_date_p0')) %>% 
    # Remove those ends by "_midd"
    select(-ends_with("_midd")) %>% 
    select(-c('readng_ver_m1', 'readng_lan_m1', 'readng_scr_m1', 'replacement_id', 'acadyear','dropout_inferred_m1', 'persist_inferred_m1')) %>% 
    # Remove those didn't have exam scores
    filter(!is.na(readng_scr_p0), gradelevel == 3) %>% 
    mutate(age_int = round(age))
  
  num_na <- data.frame(
    missing = colSums(is.na(df))
  )
  
  
  # Modeling    
  
  vars <- names(df)[sapply(df, function(x) length(unique(x)) < 10)]
  
  get_summary <- function(var){
    df %>% 
      group_by(!!sym(var)) %>% 
      summarize(mean(readng_scr_p0), 
                median(readng_scr_p0),
                count = n(),
                proportion = n()/nrow(df))
  }
  
  summary_list <- map(vars, get_summary)
  View(summary_list)
