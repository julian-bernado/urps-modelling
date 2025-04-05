# These are the formulas as they exist in `final_formulas.rds`, but de-stemmed and the averages taken out
# They're written out here for two reasons:
# (1) To be able to actually look at the variables across models
# (2) So that we can add raceth or gender to a model if it wasn't already in there 
# NOTE: After investigating the models produced by `compare_all_models.R`, the glmath 678 models were underperforming a bit
# I decided to use the glmath5 formula to fit those models and it improved the fit.
# Also, the dummy variables are coded differently in different grades
# And we add in the year and year*frl_ever_2 terms

glmath3_covars <- c(
  "enrfay_school",
  "frl_ever",
  "specialed_ever",
  "chronic_absentee_p0",
  "chronic_absentee_m1",
  "raceth",
  "gender"
)

glmath4_covars <- c(
  "glmath_scr_m1",
  "readng_scr_m1",
  "age",
  "frl_ever",
  "specialed_ever",
  "chronic_absentee_p0",
  "chronic_absentee_m1",
  "lep_ever",
  "raceth",
  "gender"
)

glmath5_covars <- c(
  "glmath_scr_m1",
  "readng_scr_m1",
  "frl_ever",
  "specialed_ever",
  "chronic_absentee_p0",
  "lep_ever",
  "raceth",
  "gender"
)

glmath6_covars <- c(
  "glmath_scr_m1",
  "readng_scr_m1",
  "frl_ever",
  "specialed_ever",
  "chronic_absentee_p0",
  "lep_ever",
  "raceth",
  "gender"
)

glmath7_covars <- c(
  "glmath_scr_m1",
  "readng_scr_m1",
  "frl_ever",
  "specialed_ever",
  "chronic_absentee_p0",
  "lep_ever",
  "raceth",
  "gender"
)

glmath8_covars <- c(
  "glmath_scr_m1",
  "readng_scr_m1",
  "frl_ever",
  "specialed_ever",
  "chronic_absentee_p0",
  "lep_ever",
  "raceth",
  "gender"
)

readng3_covars <- c(
  "enrfay_school",
  "frl_ever",
  "specialed_ever",
  "transferred_out_p0",
  "gender",
  "raceth",
  "gender"
)

readng4_covars <- c(
  "readng_scr_m1",
  "enrfay_state",
  "frl_ever",
  "specialed_ever",
  "gender",
  "raceth"
)

readng5_covars <- c(
  "readng_scr_m1",
  "enrfay_state",
  "frl_ever",
  "specialed_ever",
  "gender",
  "raceth"
)

readng6_covars <- c(
  "readng_scr_m1",
  "glmath_scr_m1",
  "age",
  "frl_ever",
  "specialed_ever",
  "chronic_absentee_p0",
  "lep_ever",
  "gender",
  "raceth"
)

readng7_covars <- c(
  "readng_scr_m1",
  "glmath_scr_m1",
  "age",
  "frl_ever",
  "specialed_ever",
  "chronic_absentee_p0",
  "chronic_absentee_m1",
  "lep_ever",
  "gender",
  "raceth"
)

readng8_covars <- c(
  "readng_scr_m1",
  "glmath_scr_m1",
  "age",
  "enrfay_school",
  "frl_ever",
  "specialed_ever",
  "chronic_absentee_p0",
  "chronic_absentee_m1",
  "lep_ever",
  "raceth",
  "gender"
)

formulas <- list(
  "glmath3" = glmath3_covars,
  "glmath4" = glmath4_covars,
  "glmath5" = glmath5_covars,
  "glmath6" = glmath6_covars,
  "glmath7" = glmath7_covars,
  "glmath8" = glmath8_covars,
  "readng3" = readng3_covars,
  "readng4" = readng4_covars,
  "readng5" = readng5_covars,
  "readng6" = readng6_covars,
  "readng7" = readng7_covars,
  "readng8" = readng8_covars
)

# Now, let's load in the data just as we did before:
# Load libraries
library(lme4)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(splines)

# Source our helper functions
source("scripts/helpers.R")

# Now, let's loop through the formulas and modify them with the following rules:
# - Any "_now" or "_2yr" or "_midd" variable should be replaced with a "_ever" variable
# - If frl_ever is not in the model, we add it
# - If this makes any duplicate variables, we remove them
# - We remove the custom `age_int` variable
# - For all remaining variables, we'll add an "avg_" version
# - Finally, we remove the `avg_frl` variable
# We're also going to collect a list of variables used in any model
all_covars <- unique(c(
  glmath3_covars,
  glmath4_covars,
  glmath5_covars,
  glmath6_covars,
  glmath7_covars,
  glmath8_covars,
  readng3_covars,
  readng4_covars,
  readng5_covars,
  readng6_covars,
  readng7_covars,
  readng8_covars
))

# We now make a list of actual columns that we'll select from the dataset
actual_columns <- c(
  all_covars,
  "glmath_scr_p0",
  "readng_scr_p0",
  "schoolid_nces_enroll_p0",
  "gradelevel"
)

# Now we can load the data and select the columns
df2019 <- read_csv("/home/rstudio/TEA_2019.csv") %>%
  select(all_of(actual_columns)) %>%
  mutate(year = 0)

df2022 <- read_csv("/home/rstudio/TEA_2022.csv") %>%
  select(all_of(actual_columns)) %>%
  mutate(year = 1)

df <- rbind(df2019, df2022)

# We're going to split the dataframe into one per grade
df_list <- list()

for (grade in 3:8) {
  df_list[[as.character(grade)]] <- df %>% 
    filter(gradelevel == grade) %>%
    select(-all_of("gradelevel"))
}

rm(df)
rm(df2019)
rm(df2022)


# Let's get all of the binary variables to be 0-1
df_list <- lapply(df_list, function(data) {
  data %>% mutate(gender = gender - 1)
})

lapply(df_list, function(x) x %>% colnames())

# Then, within each grade, we need to one-hot encode many of the variables
one_hot_variables <- c(
  "raceth",
  "transferred_out_p0"
)

# We do it for each grade
df_list <- lapply(df_list, function(x) {
  x %>% one_hot(one_hot_variables)
})

# Now we remove the generated columns that are entirely NA
df_list <- lapply(df_list, function(x){
  bad_cols <- c()
  for (col in colnames(x)){
    if (x %>% pull(col) %>% is.na() %>% all()) {
      bad_cols <- c(bad_cols, col)
    }
  }
  x %>% select(-all_of(bad_cols))
})


original_to_one_hots <- as.list(rep(NA, length(one_hot_variables)))
names(original_to_one_hots) <- one_hot_variables

for (i in 1:length(one_hot_variables)) {
  var <- one_hot_variables[i]
  one_hot_names <- list()
  for (grade in 3:8) {
    grade_c <- as.character(grade)
    one_hot_names[[grade_c]] <- df_list[[grade_c]] %>%
      select(starts_with(var)) %>%
      colnames()
  }
  original_to_one_hots[[var]] <- one_hot_names
}

# Now that we know the one hot names,
# let's go through the original formulas list
# and replace the original variable names with the one-hot names
# We can also add the "avg_" prefix to all variables now

formulas

expand_one_hot <- function(covars, grade){
  grade_c <- as.character(grade)
  if(any(grepl("raceth", covars))){
    covars <- covars[!grepl("raceth", covars)]
    covars <- c(covars, original_to_one_hots[["raceth"]][[grade_c]][-1])
  } 
  if (any(grepl("transferred_out_p0", covars))) {
    covars <- covars[!grepl("transferred_out_p0", covars)]
    covars <- c(covars, original_to_one_hots[["transferred_out_p0"]][[grade_c]][-1])
  }
  covars <- c(covars, paste0("avg_", covars))
  covars <- covars[!grepl("avg_frl_ever", covars)]
  covars
}

for(grade in 3:8){
  grade_c <- as.character("grade")
  for(subject in c("glmath", "readng")){
    formulas[[paste0(subject, grade)]] <- expand_one_hot(formulas[[paste0(subject, grade)]], grade)
  }
}


# Let's look at the variables, remove odd values, and normalize
maxlist <- list()

for(grade in 3:8){
  grade_c <- as.character(grade)
  maxlist[[grade_c]] <- df_list[[grade_c]] %>% summarise(across(everything(), ~max(.x, na.rm = TRUE))) %>% as.numeric()
}
maxlist


# Based on this, the variables with a max > 1 are:
high_max <- c("age", "glmath_scr_p0", "readng_scr_p0", "glmath_scr_m1", "readng_scr_m1")

# So we scale them by their max to get every variable to have a max of one
df_list <- lapply(df_list, function(x) {
  for (var in high_max) {
    if( var %in% colnames(x)){
      x[,var] <- x[,var]/max(x[,var], na.rm = TRUE)
    }
  }
  x
})

# Now, let's make the dataframe grouped within each grade
df_list <- lapply(df_list, function(x) {
  y <- make_grouped(x, grouping = "schoolid_nces_enroll_p0", group_level = c(), unit_level = colnames(x)[colnames(x) != "schoolid_nces_enroll_p0"])
  y$glmath_scr_p0 <- y$glmath_scr_p0 + y$avg_glmath_scr_p0
  y$readng_scr_p0 <- y$readng_scr_p0 + y$avg_readng_scr_p0
  y$frl_ever_2 <- y$frl_ever + y$avg_frl_ever
  y$year <- y$year + y$avg_year
  y
})

meanlist <- list()

for(grade in 3:8){
  grade_c <- as.character(grade)
  meanlist[[grade_c]] <- df_list[[grade_c]] %>% summarise(across(everything(), ~mean(.x, na.rm = TRUE))) %>% as.numeric()
}
# meanlist looks good

# `outcome ~ (1 | schoolid_state_enroll) + frl_ever + ns(outcome_scr_m1) + covars`
covars_to_formula <- function(covars, subject, grade) {
  grade_c <- as.character(grade)
  outcome <- paste0(subject, "_scr_p0")
  outcome_lag <- paste0(subject, "_scr_m1")
  
  # Stick in the smoothing spline for the outcome_lag
  p <- length(covars)
  for (i in 1:p) {
    if (outcome_lag == covars[i]) {
      covars[i] <- paste0("ns(", covars[i], ", df = 3)")
    }
  }
  
  # Now let's remove the first of the one hot encoding so that we're not colinear
  num_one_hot <- length(one_hot_variables)
  for (i in 1:num_one_hot) {
    if (any(grepl(one_hot_variables[i], covars))) {
      column_vals <- original_to_one_hots[[one_hot_variables[i]]][[grade_c]]
      first_val <- column_vals[1]
      covars <- covars[!grepl(first_val, covars)]
      covars <- covars[!grepl(paste0("avg_", first_val), covars)]
    }
  }

  formula_string <- paste0(
    outcome,
    " ~ (1 | schoolid_nces_enroll_p0) + year*frl_ever + year + ",
    paste0(covars, collapse = " + ")
  )
  formula_string
}

model_formulas <- formulas

for (subject in c("glmath", "readng")) {
  for (grade in 3:8){
    model_formulas[[paste0(subject, grade)]] <- covars_to_formula(formulas[[paste0(subject, grade)]], subject, grade)
  }
}

# Now we can actually fit models
# Now that we have all formulas, we can fit the models
library(lmerTest)
models <- model_formulas

for (subject in c("glmath", "readng")) {
  for (grade in 3:8) {
    key <- paste0(subject, grade)
    grade_c <- as.character(grade)
    models[[key]] <- lmer(model_formulas[[key]], data = df_list[[grade_c]], REML = FALSE)
  }
}
saveRDS(models$glmath3, "models/glmath3.rds")
saveRDS(models$glmath4, "models/glmath4.rds")
saveRDS(models$glmath5, "models/glmath5.rds")
saveRDS(models$glmath6, "models/glmath6.rds")
saveRDS(models$glmath7, "models/glmath7.rds")
saveRDS(models$glmath8, "models/glmath8.rds")

saveRDS(models$readng3, "models/readng3.rds")
saveRDS(models$readng4, "models/readng4.rds")
saveRDS(models$readng5, "models/readng5.rds")
saveRDS(models$readng6, "models/readng6.rds")
saveRDS(models$readng7, "models/readng7.rds")
saveRDS(models$readng8, "models/readng8.rds")
