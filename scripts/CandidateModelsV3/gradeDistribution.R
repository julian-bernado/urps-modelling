This file produce table summary statistics across the grades. 

Trends forr scores: 

data %>% 
  select(gradelevel, readng_scr_p0, glmath_scr_p0) %>% 
  group_by(gradelevel) %>% 
  summarize(mean_readng = mean(readng_scr_p0, na.rm = TRUE),
            median_readng = median(readng_scr_p0, na.rm = TRUE),
            mean_math = mean(glmath_scr_p0, na.rm = TRUE),
            median_math = median(glmath_scr_p0, na.rm = TRUE),)

### Boxplot
library(ggplot2)

boxplot_reading <- ggplot(data, aes(x = factor(gradelevel), y = readng_scr_p0, fill = factor(gradelevel))) +
  geom_boxplot() +
  labs(title = "Reading Score Distribution by Grade",
       x = "Grade",
       y = "Score",
       fill = "Grade") +
  theme_minimal()


boxplot_math <- ggplot(data, aes(x = factor(gradelevel), y = glmath_scr_p0, fill = factor(gradelevel))) +
  geom_boxplot() +
  labs(title = "Math Score Distribution by Grade",
       x = "Grade",
       y = "Score",
       fill = "Grade") +
  theme_minimal()


# Save the boxplot as a PNG file
ggsave(filename = "boxplot_reading.png", plot = boxplot_reading, width = 8, height = 6, dpi = 300)
ggsave(filename = "boxplot_math.png", plot = boxplot_math, width = 8, height = 6, dpi = 300)



