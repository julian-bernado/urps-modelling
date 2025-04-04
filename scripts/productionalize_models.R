# In this script, I will productionalize the models
# From "scripts/all_models.R" I will load the models
# For each (grade, subject) combination, I will test all model formulas
# I will save the best model for each combination
# I will save the model metrics for each model and plot them
source("formulas.R")

# Load libraries
library(lme4)
library(readr)
library(reshape2)
library(ggplot2)

df <- read_csv("../../tea/bernado/TEA_2019.csv")

# Loop over each item in all_formulas
for (setting_name in names(all_formulas)) {
  formulas_this_setting <- all_formulas[[setting_name]]
  metrics_list <- list()
  for (fidx in seq_along(formulas_this_setting)) {
    f_str <- formulas_this_setting[[fidx]]
    form <- as.formula(f_str)
    fit <- lmer(form, data = df, REML = FALSE)

    pred_vals <- predict(fit)
    if (grepl("math", setting_name)) {
      obs_vals <- df$glmath_scr_p0
    } else {
      obs_vals  <- df$readng_scr_p0
    }
    mse_val   <- mean((obs_vals - pred_vals)^2, na.rm = TRUE)
    aic_val <- AIC(fit)
    bic_val <- BIC(fit)
    
    # Store the results in a data frame
    metrics_list[[fidx]] <- data.frame(
      Model = paste0("Model_", fidx),
      MSE   = mse_val,
      AIC   = aic_val,
      BIC   = bic_val
    )
  }
  
  # Combine all model results for this setting into a single data frame
  metrics_df <- do.call(rbind, metrics_list)
  
  # Convert from wide to long format for grouped bar charts
  # We want columns: Model, Metric, Value
  metrics_long <- melt(
    metrics_df,
    id.vars = "Model",
    variable.name = "Metric",
    value.name = "Value"
  )
  
  p <- ggplot(metrics_long, aes(x = Model, y = Value, fill = Metric)) +
    geom_col(position = position_dodge(width = 0.7)) +
    labs(
      title = paste0("Model Comparison for ", setting_name),
      x = "Model",
      y = "Metric Value"
    ) +
    theme_minimal()
  
  out_filename <- paste0("plots/", "plot_metrics_", setting_name, ".png")
  ggsave(out_filename, plot = p, width = 7, height = 5, dpi = 300)
  print(p)
}
