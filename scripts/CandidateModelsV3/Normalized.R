Comparing normalized vs unnormalized models. 


################
# G3 Models

# Reproducibility   
```{r}
set.seed(489)
```   

Data is preprocessed as `Junjie_Reading345_V1.Rmd`

# Load Pacakges and Data    
```{r warning=FALSE}
library(readr)
library(dplyr)
library(lme4)
library(purrr)
data <- read_csv("/home/rstudio/TEA_2019.csv")
unique_schools <- unique(data$schoolid_nces_enroll_p0)

schools_sampled <- sample(unique_schools, size = round(0.3 * length(unique_schools)))


```{r}
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

```   
```{R}
num_na <- data.frame(
  missing = colSums(is.na(df))
)
```

Models_G3_R[["mod16"]] gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + chronic_absentee_m1 + readng_lan_p0 + homeless_now + migrant_now + persist_inferred_p0