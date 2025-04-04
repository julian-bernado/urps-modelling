# Create a list with 12 slots for each grade-subject combination
model_formulas <- list()

# Grade 3 Reading
model_formulas$G3_reading <- c(
  "readng_scr_p0 ~ 1 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ gender + raceth + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ gender + raceth + age_int + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ gender + raceth + age_int + frl_ever + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ gender + raceth + age_int + frl_now + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ gender + raceth + frl_now + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ gender + raceth + frl_now + lep_now + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ gender + raceth + frl_now + lep_ever + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ gender + raceth + frl_now + specialed_now + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ gender + raceth + frl_now + specialed_now + enrfay_school + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ gender + raceth + frl_now + specialed_now + enrfay_state + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 +chronic_absentee_m1 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 +chronic_absentee_m1 + readng_lan_p0 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + chronic_absentee_m1 + readng_lan_p0 + homeless_now + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + chronic_absentee_m1 + readng_lan_p0 + homeless_now + migrant_now + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + chronic_absentee_m1 + readng_lan_p0 + homeless_now + migrant_now + persist_inferred_p0 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ enrfay_school + enrfay_district + enrfay_state + age_int + gender + raceth + frl_now + lep_now + migrant_now + homeless_now + specialed_now + attend_p0_d1 + dropout_inferred_p0 + transferred_out_p0 + chronic_absentee_p0 + readng_lan_p0 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ enrfay_school + enrfay_state + age_int + gender + raceth + frl_now + lep_now + migrant_now + homeless_now + specialed_now + attend_p0_d1 + dropout_inferred_p0 + transferred_out_p0 + chronic_absentee_p0 + readng_lan_p0 + (1 | schoolid_nces_enroll_p0)"
)

# Grade 3 Math
model_formulas$G3_math <- c(
  "glmath_scr_p0 ~ 1 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ gender + raceth + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ gender + raceth + age_int + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ gender + raceth + age_int + frl_ever + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ gender + raceth + age_int + frl_now + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ gender + raceth + frl_now + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ gender + raceth + frl_now + lep_now + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ gender + raceth + frl_now + lep_ever + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ gender + raceth + frl_now + specialed_now + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ gender + raceth + frl_now + specialed_now + enrfay_school + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ gender + raceth + frl_now + specialed_now + enrfay_state + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + chronic_absentee_m1 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + chronic_absentee_m1 + glmath_lan_p0 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + chronic_absentee_m1 + glmath_lan_p0 + homeless_now + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + chronic_absentee_m1 + glmath_lan_p0 + homeless_now + migrant_now + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + chronic_absentee_m1 + glmath_lan_p0 + homeless_now + migrant_now + persist_inferred_p0 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ enrfay_school + enrfay_district + enrfay_state + age_int + gender + raceth + frl_now + lep_now + migrant_now + homeless_now + specialed_now + attend_p0_d1 + dropout_inferred_p0 + transferred_out_p0 + chronic_absentee_p0 + glmath_lan_p0 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ enrfay_school + enrfay_state + age_int + gender + raceth + frl_now + lep_now + migrant_now + homeless_now + specialed_now + attend_p0_d1 + dropout_inferred_p0 + transferred_out_p0 + chronic_absentee_p0 + glmath_lan_p0 + (1 | schoolid_nces_enroll_p0)"
)

# Grade 4 Reading
model_formulas$G4_reading <- c(
  "readng_scr_p0 ~ 1 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_now + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_now + lep_now + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + age + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + enrfay_school + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + enrfay_school + raceth + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + enrfay_school + raceth + gender + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + enrfay_school + raceth + gender + attend_p0 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + enrfay_school + raceth + gender + attend_p0 + attend_m1 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + enrfay_school + raceth + gender + attend_p0 + attend_m1 + readng_lan_m1 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + enrfay_school + raceth + gender + attend_p0 + attend_m1 + readng_lan_m1 + homeless_now + (1 | schoolid_nces_enroll_p0)"
)

# Grade 4 Math
model_formulas$G4_math <- c(
  "glmath_scr_p0 ~ glmath_scr_m1 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + readng_scr_m1 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + age_int + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + age_int + frl_ever + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + age_int + frl_ever + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + age_int + frl_now + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + age_int + frl_now + lep_now + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + age_int + frl_now + specialed_now + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + age_int + frl_now + specialed_now + enrfay_school + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + age_int + frl_now + specialed_now + enrfay_state + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + transferred_out_m1 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + chronic_absentee_m1 + glmath_lan_p0 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + chronic_absentee_m1 + glmath_lan_p0 + homeless_now + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + chronic_absentee_m1 + glmath_lan_p0 + homeless_now + migrant_now + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + chronic_absentee_m1 + glmath_lan_p0 + homeless_now + migrant_now + persist_inferred_p0 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + transferred_out_m1 + chronic_absentee_p0 + chronic_absentee_m1 + glmath_lan_p0 + glmath_lan_m1 + persist_inferred_p0 + persist_inferred_m1 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + chronic_absentee_p0 + glmath_lan_p0 + persist_inferred_p0 + (1 | schoolid_nces_enroll_p0)"
)

# Grade 5 Reading
model_formulas$G5_reading <- c(
  "readng_scr_p0 ~ 1 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_now + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_now + lep_now + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + age + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + enrfay_school + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + enrfay_school + raceth + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + enrfay_school + raceth + gender + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + enrfay_school + raceth + gender + attend_p0 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + enrfay_school + raceth + gender + attend_p0 + attend_m1 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + enrfay_school + raceth + gender + attend_p0 + attend_m1 + readng_lan_m1 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + enrfay_school + raceth + gender + attend_p0 + attend_m1 + readng_lan_m1 + homeless_now + (1 | schoolid_nces_enroll_p0)"
)

# Grade 5 Math
model_formulas$G5_math <- c(
  "glmath_scr_p0 ~ 1 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + readng_scr_m1 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + readng_scr_m1 + specialed_now + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + readng_scr_m1 + specialed_now + frl_now + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + readng_scr_m1 + specialed_now + frl_now + lep_now + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + readng_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + readng_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + age + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + readng_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + readng_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + enrfay_school + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + readng_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + enrfay_school + raceth + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + readng_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + enrfay_school + raceth + gender + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + readng_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + enrfay_school + raceth + gender + attend_p0 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + readng_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + enrfay_school + raceth + gender + attend_p0 + attend_m1 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + readng_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + enrfay_school + raceth + gender + attend_p0 + attend_m1 + glmath_lan_m1 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + readng_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + enrfay_school + raceth + gender + attend_p0 + attend_m1 + glmath_lan_m1 + homeless_now + (1 | schoolid_nces_enroll_p0)"
)

# Grade 6 Reading
model_formulas$G6_reading <- c(
  "readng_scr_p0 ~ 1 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_now + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_now + lep_now + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + age + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + enrfay_school + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + enrfay_school + raceth + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + enrfay_school + raceth + gender + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + enrfay_school + raceth + gender + attend_p0 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + enrfay_school + raceth + gender + attend_p0 + attend_m1 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + enrfay_school + raceth + gender + attend_p0 + attend_m1 + readng_lan_m1 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_now + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + enrfay_school + raceth + gender + attend_p0 + attend_m1 + readng_lan_m1 + homeless_now + (1 | schoolid_nces_enroll_p0)"
)

# Grade 6 Math
model_formulas$G6_math <- c(
  "glmath_scr_p0 ~ glmath_scr_m1 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + readng_scr_m1 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + age_int + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + age_int + frl_ever + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + age_int + frl_ever + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + age_int + frl_now + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + age_int + frl_now + lep_now + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + age_int + frl_now + specialed_now + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + age_int + frl_now + specialed_now + enrfay_school + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + age_int + frl_now + specialed_now + enrfay_state + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + transferred_out_m1 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + chronic_absentee_m1 + glmath_lan_p0 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + chronic_absentee_m1 + glmath_lan_p0 + homeless_now + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + chronic_absentee_m1 + glmath_lan_p0 + homeless_now + migrant_now + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + chronic_absentee_m1 + glmath_lan_p0 + homeless_now + migrant_now + persist_inferred_p0 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + transferred_out_m1 + chronic_absentee_p0 + chronic_absentee_m1 + glmath_lan_p0 + glmath_lan_m1 + persist_inferred_p0 + persist_inferred_m1 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + chronic_absentee_p0 + glmath_lan_p0 + persist_inferred_p0 + (1 | schoolid_nces_enroll_p0)"
)

# Grade 7 Reading
model_formulas$G7_reading <- c(
  "readng_scr_p0 ~ 1 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_2yr + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_2yr + lep_now + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_2yr + lep_now + frl_2yr + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_2yr + lep_now + frl_2yr + chronic_absentee_p0 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_2yr + lep_now + frl_2yr + chronic_absentee_p0 + age + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_2yr + lep_now + frl_2yr + chronic_absentee_p0 + age + chronic_absentee_m1 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_2yr + lep_now + frl_2yr + chronic_absentee_p0 + age + chronic_absentee_m1 + gender + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_2yr + lep_now + frl_2yr + chronic_absentee_p0 + age + chronic_absentee_m1 + gender + raceth + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_2yr + lep_now + frl_2yr + chronic_absentee_p0 + age + chronic_absentee_m1 + gender + raceth + enrfay_school + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_2yr + lep_now + frl_2yr + chronic_absentee_p0 + age + chronic_absentee_m1 + gender + raceth + enrfay_school + homeless_2yr + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_2yr + lep_now + frl_2yr + chronic_absentee_p0 + age + chronic_absentee_m1 + gender + raceth + enrfay_school + homeless_2yr + attend_p0 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_2yr + lep_now + frl_2yr + chronic_absentee_p0 + age + chronic_absentee_m1 + gender + raceth + enrfay_school + homeless_2yr + attend_p0 + transferred_out_p0 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_2yr + lep_now + frl_2yr + chronic_absentee_p0 + age + chronic_absentee_m1 + gender + raceth + enrfay_school + homeless_2yr + attend_p0 + transferred_out_p0 + attend_m1 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_2yr + lep_now + frl_2yr + chronic_absentee_p0 + age + chronic_absentee_m1 + gender + raceth + enrfay_school + homeless_2yr + attend_p0 + transferred_out_p0 + attend_m1 + transferred_out_m1 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_2yr + lep_now + frl_2yr + chronic_absentee_p0 + age + chronic_absentee_m1 + gender + raceth + enrfay_school + homeless_2yr + attend_p0 + transferred_out_p0 + attend_m1 + transferred_out_m1 + readng_lan_m1 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_2yr + lep_now + frl_2yr + chronic_absentee_p0 + age + chronic_absentee_m1 + gender + raceth + enrfay_school + homeless_2yr + attend_p0 + transferred_out_p0 + attend_m1 + transferred_out_m1 + readng_lan_m1 + migrant_2yr + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_2yr + lep_now + frl_2yr + chronic_absentee_p0 + age + chronic_absentee_m1 + gender + raceth + enrfay_school + homeless_2yr + attend_p0 + transferred_out_p0 + attend_m1 + transferred_out_m1 + readng_lan_m1 + migrant_2yr + dropout_inferred_p0 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_2yr + lep_now + frl_2yr + chronic_absentee_p0 + age + chronic_absentee_m1 + gender + raceth + enrfay_school + homeless_2yr + attend_p0 + transferred_out_p0 + attend_m1 + transferred_out_m1 + readng_lan_m1 + migrant_2yr + dropout_inferred_p0 + persist_inferred_p0 + (1 | schoolid_nces_enroll_p0)"
)

# Grade 7 Math
model_formulas$G7_math <- c(
  "glmath_scr_p0 ~ glmath_scr_m1 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + readng_scr_m1 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + age_int + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + age_int + frl_ever + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + age_int + frl_ever + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + age_int + frl_now + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + age_int + frl_now + lep_now + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + age_int + frl_now + specialed_now + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + age_int + frl_now + specialed_now + enrfay_school + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + age_int + frl_now + specialed_now + enrfay_state + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + transferred_out_m1 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + chronic_absentee_m1 + glmath_lan_p0 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + chronic_absentee_m1 + glmath_lan_p0 + homeless_now + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + chronic_absentee_m1 + glmath_lan_p0 + homeless_now + migrant_now + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + chronic_absentee_m1 + glmath_lan_p0 + homeless_now + migrant_now + persist_inferred_p0 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + transferred_out_m1 + chronic_absentee_p0 + chronic_absentee_m1 + glmath_lan_p0 + glmath_lan_m1 + persist_inferred_p0 + persist_inferred_m1 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + chronic_absentee_p0 + glmath_lan_p0 + persist_inferred_p0 + (1 | schoolid_nces_enroll_p0)"
)

# Grade 8 Reading
model_formulas$G8_reading <- c(
  "readng_scr_p0 ~ 1 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_2yr + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_2yr + lep_now + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_2yr + lep_now + chronic_absentee_p0 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_2yr + lep_now + chronic_absentee_p0 + age + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_2yr + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_2yr + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + enrfay_school + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_2yr + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + enrfay_school + raceth + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_2yr + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + enrfay_school + raceth + gender + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_2yr + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + enrfay_school + raceth + gender+ attend_p0 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_2yr + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + enrfay_school + raceth + gender+ attend_p0 + attend_m1 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_2yr + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + enrfay_school + raceth + gender+ attend_p0 + attend_m1 + readng_lan_m1 + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_2yr + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + enrfay_school + raceth + gender+ attend_p0 + attend_m1 + readng_lan_m1 + homeless_2yr + (1 | schoolid_nces_enroll_p0)",
  "readng_scr_p0 ~ readng_scr_m1 + glmath_scr_m1 + specialed_now + frl_2yr + lep_now + chronic_absentee_p0 + age + chronic_absentee_m1 + enrfay_school + raceth + gender+ attend_p0 + attend_m1 + readng_lan_m1 + homeless_2yr + migrant_now + (1 | schoolid_nces_enroll_p0)"
)

# Grade 8 Math
model_formulas$G8_math <- c(
  "glmath_scr_p0 ~ glmath_scr_m1 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + readng_scr_m1 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + age_int + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + age_int + frl_ever + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + age_int + frl_ever + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + age_int + frl_now + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + age_int + frl_now + lep_now + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + age_int + frl_now + specialed_now + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + age_int + frl_now + specialed_now + enrfay_school + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + age_int + frl_now + specialed_now + enrfay_state + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + transferred_out_m1 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + chronic_absentee_m1 + glmath_lan_p0 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + chronic_absentee_m1 + glmath_lan_p0 + homeless_now + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + chronic_absentee_m1 + glmath_lan_p0 + homeless_now + migrant_now + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + chronic_absentee_m1 + glmath_lan_p0 + homeless_now + migrant_now + persist_inferred_p0 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + transferred_out_m1 + chronic_absentee_p0 + chronic_absentee_m1 + glmath_lan_p0 + glmath_lan_m1 + persist_inferred_p0 + persist_inferred_m1 + (1 | schoolid_nces_enroll_p0)",
  "glmath_scr_p0 ~ glmath_scr_m1 + gender + raceth + frl_now + specialed_now + enrfay_school + transferred_out_p0 + chronic_absentee_p0 + glmath_lan_p0 + persist_inferred_p0 + (1 | schoolid_nces_enroll_p0)"
)