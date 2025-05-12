#' Function to sample and clean our data
#'
#' @param data The TEA data
#' @param sample Proportion of schools to sample, default 0.3
#' @return A cleaned data with school sampled
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

#' Function to create datasets for each grade and subject
#'
#' @param data The cleaned data
#' @return A list of datasets for each grade and subject of the form df_grade_subject, e.g. df_5_r
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

#' Function to get numerical summary of one categorical variable from data
#'
#' @param var Categorical variable
#' @param data The TEA data
#' @return A dataframe containing numerical summary of one categorical variable from data
get_var_summary <- function(var, data1){
  data1 %>% 
    group_by(!!sym(var)) %>% 
    summarize(mean(readng_scr_p0, na.rm = T), 
              median(readng_scr_p0, na.rm = T),
              count = n(),
              proportion = n()/nrow(data1))
}

#' Function to get numerical summary of categorical variables from data
#'
#' @param df data frame
#' @return numerical summaries of categorical variables less than 15 categories from data
get_var_summary_list <- function(df){
  vars <- names(df)[sapply(df, function(x) length(unique(x)) < 15)]
  summary_list <- map(vars, ~get_var_summary(var = .x, data1 = df))
  return(summary_list)
}


#' Function to create a list to keep lists of models
#'
#' @param
#' @return a list of lists named `c_mod_grade_subject` for future use
create_candidate_mod_list <- function(){
  c_mod_list <- list()
  for(grade in 3:8){
    for(subject in c("m", "r")){
      c_mod_list[[paste("c_mod", grade, subject, sep="_")]] <- list()
    }
  }
  return(c_mod_list)
} 

#' Function to create formulas for modeling
#'
#' @param subject `r` for reading, `m` for math
#' @param predictors predictors to be written in formula
#' @return a formula to be passed in `lmer` function
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

#' Function to generate candidate models for a given subject and grade
#'
#' @param candidate_vars candidate variables whose combination will be modeled
#' @param fixed_vars fixed variables that will be included in every model
#' @param subject `r` for reading, `m` for math
#' @param grade an integer from 3-8
#' @param list1 a list to save models, default `c_mod_list`
#' @return candidate models passed into the `c_mod_grade_subject` list in the `c_mod_list`
create_candidate_mod <- function(candidate_vars, fixed_vars, subject, grade, list1) {
  all_combinations <- map(0:length(candidate_vars), ~ combn(candidate_vars, m = .x, simplify = FALSE)) |> flatten()
  
  formulas <- map(all_combinations, ~ create_formula(subject = subject, predictors = c(fixed_vars, .x)))
  
  list1[[paste("c_mod", grade, subject, sep="_")]] <- 
    map(formulas, ~ lmer(.x, data = dfs[[paste("df", grade, subject, sep = "_")]], REML = FALSE))
  
  return(list1)
}


#' Function to generate candidate models for a given subject and grade
#'
#' @param candidate_vars candidate variables whose combination will be modeled
#' @param fixed_vars fixed variables that will be included in every model
#' @param subject `r` for reading, `m` for math
#' @param grade an integer from 3-8
#' @return candidate models passed into the `c_mod_grade_subject` list in the `c_mod_list`
model_selection <- function(grade, subject, list1){
  list_name <- paste("c_mod", grade, subject, sep="_")
  aic_values <- map_dbl(list1[[list_name]], AIC)
  bic_values <- map_dbl(list1[[list_name]], BIC)
  mse_values <- map_dbl(list1[[list_name]], function(model) {
    preds <- predict(model)
    truth <- model@frame[[1]]
    mean((preds - truth)^2)
  })
  
  # Combine everything nicely into a tibble
  comparison_table <- tibble(
    model_id = seq_along(list1[[list_name]]),
    AIC = aic_values,
    BIC = bic_values,
    MSE = mse_values
  )
  
  print(comparison_table)
  
  comparison_table <- comparison_table %>%
    arrange(MSE) %>%
    mutate(mse_rank = row_number()) %>% 
    slice(1:50) %>% 
    mutate(
      rank_aic = rank(AIC, ties.method = "first"),
      rank_bic = rank(BIC, ties.method = "first"),
      mean_rank = (rank_aic + rank_bic) / 2
    ) %>% 
    arrange(mean_rank)
  
  return(comparison_table)
}
