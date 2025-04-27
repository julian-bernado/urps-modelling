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

get_var_summary <- function(var){
  df %>% 
    group_by(!!sym(var)) %>% 
    summarize(mean(readng_scr_p0), 
              median(readng_scr_p0),
              count = n(),
              proportion = n()/nrow(df))
}

get_var_summary_list <- function(df){
  vars <- names(df)[sapply(df, function(x) length(unique(x)) < 10)]
  summary_list <- map(vars, get_summary)
  return(summary_list)
}

### We want to create a list of candidate models for each grade and subject
### We can have two lists, one for reading, another for math

create_candidate_mod_list <- function(){
  c_mod_list <- list()
  for(grade in 3:8){
    for(subject in c("m", "r")){
      c_mod_list[[paste("c_mod", grade, subject, sep="_")]] <- list()
    }
  }
  return(c_mod_list)
} 

create_formula <- function(subject, predictors){
  if(!(subject %in% c("m", "r"))){
    stop("Subject should be m or r")
  }
  if(subject == "m"){
    response <- "glmath_scr_p0"
  } else {
    response <- "readng_scr_p0"
  }
  fixed_part <- paste(predictors, collapse = " + ")
  formula <- as.formula(paste0(response, " ~ ", fixed_part, " + (1 | schoolid_nces_enroll_p0)"))
  return(formula)
}


create_candidate_mod <- function(candidate_vars, fixed_vars, subject, grade){
  all_combinations <- map(1:length(candidate_vars), ~ combn(candidate_vars, m = .x, simplify = FALSE)) |> flatten()
  formulas <- map(all_combinations, ~ create_formula(subject = subject, predictors = c(fixed_vars, .x)))
  
  
  
  c_mod_list[[paste("c_mod", grade, subject, sep="_")]] <<- 
    map(formulas, ~ lmer(.x, data = dfs[[paste("df", grade, subject, sep = "_")]],REML = FALSE))
}





