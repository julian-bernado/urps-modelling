set.seed(489)

data <- read_csv("/home/rstudio/TEA_2019.csv")
unique_schools <- unique(data$schoolid_nces_enroll_p0)
schools_sampled <- sample(unique_schools, size = round(0.3 * length(unique_schools)))
data_sampled <- data %>% 
  filter(schoolid_nces_enroll_p0 %in% schools_sampled)


migrant <- data_sampled %>% 
  select(gradelevel, readng_scr_p0, migrant_now, glmath_scr_p0) %>% 
  group_by(gradelevel, migrant_now) %>% 
  summarize(mean_readng = mean(readng_scr_p0, na.rm = TRUE),
            median_readng = median(readng_scr_p0, na.rm = TRUE),
            mean_math = mean(glmath_scr_p0, na.rm = TRUE),
            median_math = median(glmath_scr_p0, na.rm = TRUE),)

View(migrant)


library(ggplot2)
boxplot_reading_migrant <- ggplot(data_sampled, aes(x = factor(gradelevel), y = readng_scr_p0, fill = factor(migrant_now))) +
  geom_boxplot() +
  labs(title = "Reading Score across Gradelevel by Migrant_now",
       x = "Grade",
       y = "Score",
       fill = "Migrant_now") +
  theme_minimal()

boxplot_math_migrant <- ggplot(data_sampled, aes(x = factor(gradelevel), y = glmath_scr_p0, fill = factor(migrant_now))) +
  geom_boxplot() +
  labs(title = "Math Score across Gradelevel by Migrant_now",
       x = "Grade",
       y = "Score",
       fill = "Migrant_now") +
  theme_minimal()

# Save the boxplot as a PNG file
ggsave(filename = "boxplot_reading_migrant.png", plot = boxplot_reading_migrant, width = 8, height = 6, dpi = 300)
ggsave(filename = "boxplot_math_migrant.png", plot = boxplot_math_migrant, width = 8, height = 6, dpi = 300)

##Check the strange pattern
data_sampled %>%
  filter(gradelevel == 8, migrant_now == 0) %>%
  summarise(
    n = n(),
    mean = mean(glmath_scr_p0, na.rm=TRUE),
    median = median(glmath_scr_p0, na.rm=TRUE),
    Q1 = quantile(glmath_scr_p0, .25, na.rm=TRUE),
    Q3 = quantile(glmath_scr_p0, .75, na.rm=TRUE),
    IQR = IQR(glmath_scr_p0, na.rm=TRUE),
    min = min(glmath_scr_p0, na.rm=TRUE),
    max = max(glmath_scr_p0, na.rm=TRUE)
  )

## n     mean   median  Q1    Q3    IQR   min   max
##122825 1572.   1700  1043  1776   733  1043  2221

df_migrant_math <- data_sampled %>% 
  select(gradelevel,migrant_now, glmath_scr_p0) %>% 
  na.omit()

hist_math_g8_migrant0 <- df_migrant_math %>%
  filter(gradelevel == 8,migrant_now == 0) %>%
  ggplot(aes(x = glmath_scr_p0)) +
  geom_histogram(bins = 100) +
  labs(
    title = "Grade 8 Math Scores (migrant_now = 0)",
    x = "Score",
    y = "Count"
  ) +
  theme_minimal()

ggsave(filename = "hist_math_g8_migrant0.png", plot = hist_math_g8_migrant0, width = 8, height = 6, dpi = 300)


We see a huge difference inn math scores.  This is very interesting. Why? 
  
  
df_migrant_read <- data_sampled %>% 
  select(gradelevel,migrant_now, readng_scr_p0) %>% 
  na.omit()

hist_read_g8_migrant0 <- df_migrant_read %>%
  filter(gradelevel == 8,migrant_now == 0) %>%
  ggplot(aes(x = readng_scr_p0)) +
  geom_histogram(bins = 100) +
  labs(
    title = "Grade 8 Math Scores (migrant_now = 0)",
    x = "Score",
    y = "Count"
  ) +
  theme_minimal()

ggsave(filename = "hist_read_g8_migrant0.png", plot = hist_read_g8_migrant0, width = 8, height = 6, dpi = 300)



  df_migrant_math %>%
    filter(gradelevel == 3,migrant_now == 0) %>%
    ggplot(aes(x = glmath_scr_p0)) +
    geom_histogram(bins = 100) +
    labs(
      x = "Score",
      y = "Count"
    ) +
    theme_minimal()
  
  df_migrant_math %>%
    filter(gradelevel == 4,migrant_now == 0) %>%
    ggplot(aes(x = glmath_scr_p0)) +
    geom_histogram(bins = 100) +
    labs(
      x = "Score",
      y = "Count"
    ) +
    theme_minimal()
  
  df_migrant_math %>%
    filter(gradelevel == 5,migrant_now == 0) %>%
    ggplot(aes(x = glmath_scr_p0)) +
    geom_histogram(bins = 100) +
    labs(
      x = "Score",
      y = "Count"
    ) +
    theme_minimal()

  df_migrant_math %>%
    filter(gradelevel == 6,migrant_now == 0) %>%
    ggplot(aes(x = glmath_scr_p0)) +
    geom_histogram(bins = 100) +
    labs(
      x = "Score",
      y = "Count"
    ) +
    theme_minimal()
  
  df_migrant_math %>%
    filter(gradelevel == 7,migrant_now == 0) %>%
    ggplot(aes(x = glmath_scr_p0)) +
    geom_histogram(bins = 100) +
    labs(
      x = "Score",
      y = "Count"
    ) +
    theme_minimal()