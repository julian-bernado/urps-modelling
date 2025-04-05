# In this script, I will productionalize the models
# From "scripts/all_models.R" I will load the models
# For each (grade, subject) combination, I will test all model formulas
# I will save the best model for each combination
# I will save the model metrics for each model and plot them

# Load libraries
library(lme4)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(splines)

# Source our helper functions
source("scripts/helpers.R")

# First we read in the formulas
formulas <- readRDS("formulas/all_formulas.rds")

# Now, let's loop through the formulas and modify them with the following rules:
# - Any "_now" or "_2yr" or "_midd" variable should be replaced with a "_ever" variable
# - If frl_ever is not in the model, we add it
# - If this makes any duplicate variables, we remove them
# - We remove the custom `age_int` variable
# - For all remaining variables, we'll add an "avg_" version
# - Finally, we remove the `avg_frl` variable
# We're also going to collect a list of variables used in any model
all_covars <- c()
for (subject in c("glmath", "readng")) {
  for (grade in 3:8){
    all_models <- formulas[[paste0(subject, grade)]]
    n_models <- length(all_models)
    for(i in 1:n_models) {
      # Replace "_now" or "_2yr" or "_midd" with "_ever"
      all_models[[i]] <- gsub("_now|_2yr|_midd", "_ever", all_models[[i]])

      # Add in frl_ever
      all_models[[i]] <- c(all_models[[i]], "frl_ever")
      
      # Remove custom age_int variable
      all_models[[i]] <- all_models[[i]][all_models[[i]] != "age_int"]

      # Remove duplicate variables
      all_models[[i]] <- unique(all_models[[i]])
      all_covars <- c(all_covars, all_models[[i]])

      # Save in the formulas list
      formulas[[paste0(subject, grade)]][[paste0("mod", i-1)]] <- all_models[[i]]
    }
  }
}

all_covars <- unique(all_covars)

# We now make a list of actual columns that we'll select from the dataset
actual_columns <- all_covars[!grepl("avg_", all_covars)]
actual_columns <- actual_columns[actual_columns != "1"]
actual_columns <- c(
  actual_columns,
  "glmath_scr_p0",
  "readng_scr_p0",
  "schoolid_nces_enroll_p0",
  "gradelevel"
)

# Now we can load the data and select the columns
df <- read_csv("/home/rstudio/TEA_2019.csv") %>%
  select(all_of(actual_columns))

# We're going to split the dataframe into one per grade
df_list <- list()

for (grade in 3:8) {
  df_list[[as.character(grade)]] <- df %>% 
    filter(gradelevel == grade) %>%
    select(-all_of("gradelevel"))
}

# Then, within each grade, we need to one-hot encode many of the variables
one_hot_variables <- c(
  "frl_ever",
  "specialed_ever",
  "chronic_absentee_p0",
  "chronic_absentee_m1",
  "lep_ever",
  "homeless_ever",
  "transferred_out_p0",
  "gender",
  "migrant_ever",
  "persist_inferred_p0",
  "dropout_inferred_p0",
  "transferred_out_m1",
  "glmath_lan_p0",
  "readng_lan_p0",
  "glmath_lan_m1",
  "readng_lan_m1",
  "raceth"
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

for (subject in c("glmath", "readng")) {
  for (grade in 3:8){
    all_models <- formulas[[paste0(subject, grade)]]
    n_models <- length(all_models)
    for (i in 1:n_models) {
      current_vars <- all_models[[i]]
      for (var in one_hot_variables) {
        if (var %in% current_vars) {
          one_hot_names <- original_to_one_hots[[var]][[as.character(grade)]]
          current_vars <- current_vars[current_vars != var]
          current_vars <- c(current_vars, one_hot_names)
        }
      }
      current_vars <- c(current_vars, paste0("avg_", current_vars))
      current_vars <- current_vars[!grepl("avg_frl", current_vars)]
      current_vars <- current_vars[current_vars != "avg_1"]
      all_models[[i]] <- current_vars
      
      # Save in the formulas list
      formulas[[paste0(subject, grade)]][[paste0("mod", i-1)]] <- all_models[[i]]
    }
  }
}

# Let's look at the variables, remove odd values, and normalize
maxlist <- list()

for(grade in 3:8){
  grade_c <- as.character(grade)
  maxlist[[grade_c]] <- df_list[[grade_c]] %>% summarise(across(everything(), ~max(.x, na.rm = TRUE))) %>% as.numeric()
}

# Based on this, the variables with a max > 1 are:
high_max <- c("age", "attend_p0", "attend_m1", "glmath_scr_p0", "readng_scr_p0", "glmath_scr_m1", "readng_scr_m1")

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
  y$frl_ever_2 <- y$frl_ever_2 + y$avg_frl_ever_2
  y
})

meanlist <- list()

for(grade in 3:8){
  grade_c <- as.character(grade)
  meanlist[[grade_c]] <- df_list[[grade_c]] %>% summarise(across(everything(), ~mean(.x, na.rm = TRUE))) %>% as.numeric()
}

# meanlist 
# Looks good, data should be ready now

# Now we're going to go through the all_models list
# and create a list of formulas
# The formulas will be of the form:
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
    " ~ (1 | schoolid_nces_enroll_p0) + ",
    paste0(covars, collapse = " + ")
  )
  formula_string
}

for (subject in c("glmath", "readng")) {
  for (grade in 3:8){
    all_models <- formulas[[paste0(subject, grade)]]
    n_models <- length(all_models)
    for (i in 1:n_models) {
      formulas[[paste0(subject, grade)]][[paste0("mod", i-1)]] <- covars_to_formula(all_models[[i]], subject, grade)
    }
  }
}

# Now that we have all formulas, we can fit the models
models <- formulas

for (subject in c("glmath", "readng")) {
  for (grade in 3:8) {
    print(paste0(subject, grade))
    cur_data <- df_list[[as.character(grade)]]
    all_models <- formulas[[paste0(subject, grade)]]
    n_models <- length(all_models)
    for (i in 1:n_models) {
      cur_formula <- formulas[[paste0(subject, grade)]][[paste0("mod", i-1)]]
      models[[paste0(subject, grade)]][[paste0("mod", i-1)]] <- lmer(formula = cur_formula, data = cur_data, REML = FALSE)
    }
  }
}

# Let's get the evaluation metrics from the models

model_evals <- list()
for (subject in c("glmath", "readng")) {
  for (grade in 3:8) {
    all_models <- formulas[[paste0(subject, grade)]]
    n_models <- length(all_models)
    aic_values <- numeric(length = n_models)
    bic_values <- numeric(length = n_models)
    rmse_values <- numeric(length = n_models)
    for (i in 1:n_models) {
      cur_model <- models[[paste0(subject, grade)]][[paste0("mod", i-1)]] 
      aic <- AIC(cur_model)
      bic <- BIC(cur_model)
      rmse <- mean(residuals(cur_model)^2)^(1/2)
      aic_values[i] <- aic
      bic_values[i] <- bic
      rmse_values[i] <- rmse
    }
    eval_df <- data.frame(
      model_num = 0:(n_models - 1),
      aic = aic_values,
      bic = bic_values,
      rmse = rmse_values
    )
    
    model_evals[[paste0(subject, grade)]] <- eval_df
  }
}

model_evals[["glmath3"]]

# And plot them
for (subject in c("glmath", "readng")) {
  for (grade in 3:8) {
    key <- paste0(subject, grade)
    eval_df <- model_evals[[key]]
    aic_plot <- ggplot(eval_df, aes(x = model_num, y = aic)) +
      geom_vline(xintercept = 0:nrow(eval_df), linetype = "dashed", color = "gray80") +
      geom_line() +
      geom_point() +
      labs(title = "AIC by Model Number",
           x = "Model Number",
           y = "AIC")
    bic_plot <- ggplot(eval_df, aes(x = model_num, y = bic)) +
      geom_vline(xintercept = 0:nrow(eval_df), linetype = "dashed", color = "gray80") +
      geom_line() +
      geom_point() +
      labs(title = "BIC by Model Number",
           x = "Model Number",
           y = "BIC")
    rmse_plot <- ggplot(eval_df, aes(x = model_num, y = rmse)) +
      geom_vline(xintercept = 0:nrow(eval_df), linetype = "dashed", color = "gray80") +
      geom_line() +
      geom_point() +
      labs(title = "RMSE by Model Number",
           x = "Model Number",
           y = "RMSE")
    
    ggsave(plot = aic_plot, paste0(key, "_", "aic_plot.png"))
    ggsave(plot = bic_plot, paste0(key, "_", "bic_plot.png"))
    ggsave(plot = rmse_plot, paste0(key, "_", "rmse_plot.png"))
  }
}


# Looking at the plots, I'm going to extract the following models


final_models <- list(
  glmath3 = models[["glmath3"]][["mod6"]],
  glmath4 = models[["glmath4"]][["mod9"]],
  glmath5 = models[["glmath5"]][["mod6"]],
  glmath6 = models[["glmath6"]][["mod5"]],
  glmath7 = models[["glmath7"]][["mod13"]],
  glmath8 = models[["glmath8"]][["mod10"]],
  readng3 = models[["readng3"]][["mod11"]],
  readng4 = models[["readng4"]][["mod10"]],
  readng5 = models[["readng5"]][["mod10"]],
  readng6 = models[["readng6"]][["mod9"]],
  readng7 = models[["readng7"]][["mod9"]],
  readng8 = models[["readng8"]][["mod9"]]
)

saveRDS(model_evals, "evals/model_evals.rds")
saveRDS(final_models$glmath3, "models/glmath3_model.rds")
saveRDS(final_models$glmath4, "models/glmath4_model.rds")
saveRDS(final_models$glmath5, "models/glmath5_model.rds")
saveRDS(final_models$glmath6, "models/glmath6_model.rds")
saveRDS(final_models$glmath7, "models/glmath7_model.rds")
saveRDS(final_models$glmath8, "models/glmath8_model.rds")
saveRDS(final_models$readng3, "models/readng3_model.rds")
saveRDS(final_models$readng4, "models/readng4_model.rds")
saveRDS(final_models$readng5, "models/readng5_model.rds")
saveRDS(final_models$readng6, "models/readng6_model.rds")
saveRDS(final_models$readng7, "models/readng7_model.rds")
saveRDS(final_models$readng8, "models/readng8_model.rds")

formula_to_covars <- function(f){
  strsplit(as.character(f)[3], split="+", fixed = TRUE)[[1]][-1]
}

final_formulas <- lapply(final_models, function(x){
  formula_to_covars(formula(x))
})

saveRDS(final_formulas, "formulas/final_formulas.rds")