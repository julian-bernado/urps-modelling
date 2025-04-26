################################################################################
### We need a function to generate all datasets for us. 
### Each dataset should be cleaned. 

create_dataset <- function(data, sample = 0.3){
  set.seed(489)
  unique_schools <- unique(data$schoolid_nces_enroll_p0)
  schools_sampled <- sample(unique_schools, 
                            size = round(sample * length(unique_schools)))
  
  df <- data %>% 
    filter(schoolid_nces_enroll_p0 %in% schools_sampled) %>% 
    # Remove variables with only NA's
    select(where(~ !all(is.na(.)))) %>% 
    mutate(age_int = round(age), 
           attend_p0_d1 = round(attend_p0, 1),
           attend_m1_d1 = round(attend_m1, 1))
           
  return(df)
}

clean_dataset <- function(data){
  dfs <- list()
  for(grade in 3:8){
    for(subject in c("m", "r")){
      cols_to_drop_all <- c('glmath_alt_scr_m1', 'glmath_alt_scr_p0',
                            'readng_alt_scr_m1', 'readng_alt_scr_p0',
                            'replacement_id', 'acadyear')
      cols_to_drop_subject <- if (subject == "r") {
        c('glmath_ver_p0', 'glmath_lan_p0', 'glmath_scr_p0')
      } else {
        c('readng_ver_p0', 'readng_lan_p0', 'readng_scr_p0')
      }
      dfs[[paste("df", grade, subject, sep="_")]] <- data %>% 
        select(where(~ !all(is.na(.)))) %>% 
        select(-any_of(cols_to_drop_all)) %>% 
        select(-any_of(cols_to_drop_subject)) %>% 
        select(
          if (grade < 6) {
            any_of(names(data)[!stringr::str_ends(names(data), "_midd")])
          } else {
            everything()
          }
        )%>% 
        filter(gradelevel == grade)
    }
  }
  return(dfs)
}







