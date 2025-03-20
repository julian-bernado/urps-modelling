Before doing any modeling, we first look at enrfay versus dropped out

enrfay_district
enrfay_school
enrfay_state

colnames(data)
dropout_inferred_m1 dropout_inferred_p0
Indicates whether or not the student dropped out of school, as inferred from state reporting and subsequent enrollment information.


persist_inferred_m1 persist_inferred_p0
Indicates whether or not the student persisted (remained enrolled) in school, as inferred from state reporting and subsequent enrollment information, one year prior.

df_drop <- data %>% 
  select(dropout_inferred_m1, dropout_inferred_p0, persist_inferred_m1, persist_inferred_p0) %>% 
  na.omit() %>% 
  mutate(match_m1 = (dropout_inferred_m1 == 1 - persist_inferred_m1),
         match_p0 = (dropout_inferred_p0 == 1 - persist_inferred_p0))

all(df_drop$match_m1 == TRUE)
all(df_drop$match_p0 == TRUE)
  

Check NAs

df_drop_na <- data %>% 
  select(dropout_inferred_m1, dropout_inferred_p0, persist_inferred_m1, persist_inferred_p0) %>% 
  filter(if_any(c(dropout_inferred_m1, dropout_inferred_p0, persist_inferred_m1, persist_inferred_p0), is.na)) %>% 
  mutate(match_m1 = (dropout_inferred_m1 == 1 - persist_inferred_m1) | if_all(c(dropout_inferred_m1, persist_inferred_m1), is.na),
         match_p0 = (dropout_inferred_p0 == 1 - persist_inferred_p0) | if_all(c(dropout_inferred_p0, persist_inferred_p0), is.na))

all(df_drop_na$match_m1 == TRUE)
all(df_drop_na$match_p0 == TRUE)

table(data$dropout_inferred_m1, data$persist_inferred_m1)
table(data$dropout_inferred_p0, data$persist_inferred_p0)


We conclude persist is equivalent to dropout. Now lets investigate dropout and enrfay. 
enrfay_school.   dropout_inferred_p0 

df_enrfay <- data %>% 
  select(enrfay_school, dropout_inferred_p0) %>% 
  na.omit() %>% 
  filter(enrfay_school == 1) %>% 
  filter(!(df_enrfay$enrfay_school == (1 + df_enrfay$dropout_inferred_p0)))

sum(!(df_enrfay$enrfay_school == (1 + df_enrfay$dropout_inferred_p0)))


20227 rows enrfay_school = 1 and dropout_inferred_p0 = 1. Is this an error? 

table(data$enrfay_school, data$dropout_inferred_p0)


table(data$dropout_inferred_p0, data$transferred_out_p0)
transferred + droppedout = all

Is it possible that enrfay is related to both dropout and transfer? 
Not the case. 
table(data$enrfay_school, data$dropout_inferred_p0, data$transferred_out_p0)

There is a vaiable withdrawal_data_p0 which we have not consider. Let us look at this. 
df_withdraw <- data %>% 
  select(withdrawal_date_p0, dropout_inferred_p0, transferred_out_p0, enrfay_school) %>% 
  mutate(withdrawal_p0 = !is.na(withdrawal_date_p0))

table(df_withdraw$transferred_out_p0, df_withdraw$withdrawal_p0)
table(df_withdraw$enrfay_school, df_withdraw$withdrawal_p0,df_withdraw$dropout_inferred_p0)