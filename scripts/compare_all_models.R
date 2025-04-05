# In this script, I will productionalize the models
# From "scripts/all_models.R" I will load the models
# For each (grade, subject) combination, I will test all model formulas
# I will save the best model for each combination
# I will save the model metrics for each model and plot them

# Load libraries
library(lme4)
library(readr)
library(ggplot2)

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
      formulas[[paste0(subject, grade)]][[paste0("mod", i)]] <- all_models[[i]]
    }
  }
}

all_covars <- unique(all_covars)
formulas


# We now make a list of actual columns that we'll select from the dataset
actual_columns <- all_covars[!grepl("avg_", all_covars)]
actual_columns <- actual_columns[actual_columns != "1"]
actual_columns <- c(
  actual_columns,
  "glmath_scr_p0",
  "readng_scr_p0",
  "schoolid_state_enroll_p0",
  "gradelevel"
)

# Now we can load the data and select the columns
df <- read_csv("/home/rstudio/TEA_2019.csv") %>%
  select(all_of(actual_columns))

# We're going to split the dataframe into one per grade

df_list <- list()

df_list <- lapply(df_list, function(x) {
  x %>%
    filter(gradelevel == grade) %>%
    select(-all_of("gradelevel"))
})

# Then, within each grade, we need to one-hot encode many of the variables
one_hot_variables <- c(
  "frl_ever",
  "specialed_ever",
  "chronic_absentee_p0",
  "chronic_absentee_m1",
  "enrfay_school",
  "lep_ever",
  "homeless_ever",
  "transferred_out_p0",
  "attend_m1",
  "gender",
  "migrant_ever",
  "persist_inferred_p0",
  "dropout_inferred_p0",
  "transferred_out_m1",
  "enrfay_state",
  "glmath_lan_p0",
  "readng_lan_p0",
  "glmath_lan_m1"
)

df_list <- lapply(df_list, function(x) {
  x %>% one_hot(one_hot_variables)
})

original_to_one_hots <- as.list(rep(NA, length(one_hot_variables)))
names(original_to_one_hots) <- one_hot_variables

for (var in one_hot_variables) {
  original_to_one_hots[[var]] <- df_list[["3"]] %>%
    select(starts_with(var)) %>%
    colnames()
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
          one_hot_names <- original_to_one_hots[[var]]
          current_vars <- current_vars[current_vars != var]
          current_vars <- c(current_vars, one_hot_names)
        }
      }
      current_vars <- c(current_vars, paste0("avg_", current_vars))
      current_vars <- current_vars[current_vars != "avg_frl_ever"]
      current_vars <- current_vars[current_vars != "avg_1"]
      all_models[[i]] <- current_vars
      
      # Save in the formulas list
      formulas[[paste0(subject, grade)]][[paste0("mod", i)]] <- all_models[[i]]
    }
  }
}

# Let's look at the variables, remove odd values, and normalize

# Now, let's make the dataframe grouped within each grade


# Now we're going to go through the all_models list
# and create a list of formulas
# The formulas will be of the form:
# `outcome ~ (1 | schoolid_state_enroll) + frl_ever + ns(outcome_scr_m1) + covars`

covars_to_formula <- function(covars, subject) {
  outcome <- paste0(subject, "_scr_p0")
  outcome_lag <- paste0(subject, "_scr_m1")
  p <- length(covars)
  for (i in 1:p) {
    if (grepl(outcome_lag, covars[i])) {
      covars[i] <- paste0("ns(", covars[i], ", df = 3)")
    }
  }

  num_one_hot <- length(one_hot_variables)
  for (i in 1:num_one_hot) {
    if (grepl(one_hot_variables[i], covars)) {
      covars <- covars[!grepl(paste0(one_hot_variables[i], "_1"), covars)]
      covars <- covars[!grepl(paste0("avg_", one_hot_variables[i], "_1"), covars)]
    }
  }

  formula_string <- paste0(
    outcome,
    " ~ (1 | schoolid_state_enroll_p0) + ",
    paste0(covars, collapse = " + ")
  )
  formula_string
}

for (subject in c("glmath", "readng")) {
  for (grade in 3:8){
    all_models <- formulas[[paste0(subject, grade)]]
    n_models <- length(all_models)
    for (i in 1:n_models) {
      formulas[[paste0(subject, grade)]][[paste0("mod", i)]] <- covars_to_formula(all_models[[i]], subject)
    }
  }
}

print(formulas)