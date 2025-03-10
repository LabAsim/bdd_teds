
source("data_wrangling.R")
library(umx)
library(lavaan)
################
# Hypothesis 1 #
################
#To test hypothesis 1,
# we will use Structural Equation Modeling (SEM) 
# in order to determine the longitudinal phenotypic association 
# of earlier peer victimization (MPVS score) with later BDD symptoms (DCQ score). 
# Potential confounders, namely age and sex 


# Complete cases analysis # 

df_1_modified_complete_cases <-  df_1 %>%
  fix_different_twins_values(var="age_phase2_child_21_1") %>%
  fix_different_twins_values(var="age_16_1")

df_1_modified_complete_cases <- df_1_modified_complete_cases %>%
  drop_identical_values(
      var="mpvs_total_12_1", drop_same_value=T, drop_na = T
    ) %>%
  drop_identical_values(
    var="mpvs_total_21_phase_2_1", drop_same_value=T, drop_na = T
  ) %>%
  drop_identical_values(
    var="mpvs_total_16_1", drop_same_value=T, drop_na = T
  ) %>%
  drop_identical_values(
    var="mpvs_total_14_child_1", drop_same_value=T, drop_na = T
  ) %>%
  drop_identical_values(
    var="dcq_26_1", drop_same_value=T, drop_na = T
  )

df_1_modified_complete_cases %>% select(
  twin_id, fam_id, dcq_26_1, mpvs_total_12_1, mpvs_total_14_child_1,
  mpvs_total_16_1, mpvs_total_21_phase_2_1, age_12_1,age_14_1, age_16_1,
  age_phase2_child_21_1
) %>% View()

within_fam_model <- '
  level: 1
    dcq_26_1 ~ mpvs_total_12_1
    dcq_26_1 ~ mpvs_total_14_child_1
    dcq_26_1 ~ mpvs_total_16_1
    dcq_26_1 ~ mpvs_total_21_phase_2_1
    mpvs_total_14_child_1 ~ mpvs_total_12_1
    mpvs_total_16_1 ~ mpvs_total_14_child_1
    mpvs_total_21_phase_2_1 ~ mpvs_total_16_1
  level: 2
    dcq_26_1 ~ mpvs_total_12_1
    dcq_26_1 ~ mpvs_total_14_child_1
    dcq_26_1 ~ mpvs_total_16_1
    dcq_26_1 ~ mpvs_total_21_phase_2_1
    mpvs_total_14_child_1 ~ mpvs_total_12_1
    mpvs_total_16_1 ~ mpvs_total_14_child_1
    mpvs_total_21_phase_2_1 ~ mpvs_total_16_1
    mpvs_total_12_1 ~ age_12_1
    mpvs_total_14_child_1 ~ age_14_1
    mpvs_total_16_1 ~ age_16_1
    mpvs_total_21_phase_2_1 ~ age_phase2_child_21_1
'
fit1 <- sem(
  model = within_fam_model, 
  data = df_1_modified_complete_cases, 
  cluster = "fam_id"
)

summary(fit1)


# Retain NA values # 

df_1_modified <-  df_1 %>%
  fix_different_twins_values(var="age_phase2_child_21_1") %>%
  fix_different_twins_values(var="age_12_1") %>%
  fix_different_twins_values(var="age_14_1") %>%
  fix_different_twins_values(var="age_16_1") 
  

df_1_modified <- df_1_modified %>%
  drop_identical_values(
    var="mpvs_total_12_1", drop_same_value=T, drop_na = F
  ) %>%
  drop_identical_values(
    var="mpvs_total_21_phase_2_1", drop_same_value=T, drop_na = F
  ) %>%
  drop_identical_values(
    var="mpvs_total_16_1", drop_same_value=T, drop_na = F
  ) %>%
  drop_identical_values(
    var="mpvs_total_14_child_1", drop_same_value=T, drop_na = F
  ) %>%
  drop_identical_values(
    var="dcq_26_1", drop_same_value=T, drop_na = F
  )

df_1_modified %>% select(
  twin_id, fam_id, dcq_26_1, mpvs_total_12_1, mpvs_total_14_child_1,
  mpvs_total_16_1, mpvs_total_21_phase_2_1, age_12_1,age_14_1, age_16_1,
  age_phase2_child_21_1
) %>% View()

df_1_modified %>% 
  select(
    dcq_26_1, mpvs_total_12_1, mpvs_total_14_child_1,
    mpvs_total_16_1, mpvs_total_21_phase_2_1, age_12_1,age_14_1, age_16_1,
    age_phase2_child_21_1
  ) %>% 
  mice::md.pattern(rotate.names = T, plot = F) %>% 
  View()

two_level_model <- '
  level: 1
    dcq_26_1 ~ mpvs_total_12_1
    dcq_26_1 ~ mpvs_total_14_child_1
    dcq_26_1 ~ mpvs_total_16_1
    dcq_26_1 ~ mpvs_total_21_phase_2_1
    mpvs_total_14_child_1 ~ mpvs_total_12_1
    mpvs_total_16_1 ~ mpvs_total_14_child_1
    mpvs_total_21_phase_2_1 ~ mpvs_total_16_1
  level: 2
    dcq_26_1 ~ mpvs_total_12_1
    dcq_26_1 ~ mpvs_total_14_child_1
    dcq_26_1 ~ mpvs_total_16_1
    dcq_26_1 ~ mpvs_total_21_phase_2_1
    mpvs_total_14_child_1 ~ mpvs_total_12_1
    mpvs_total_16_1 ~ mpvs_total_14_child_1
    mpvs_total_21_phase_2_1 ~ mpvs_total_16_1
    mpvs_total_12_1 ~ age_12_1
    mpvs_total_14_child_1 ~ age_14_1
    mpvs_total_16_1 ~ age_16_1
    mpvs_total_21_phase_2_1 ~ age_phase2_child_21_1
'

fit_msem_complete_cases <- sem(
  model = two_level_model, 
  data = df_1_modified, 
  cluster = "fam_id"
)

summary(fit_msem_complete_cases)

fit_msem_fiml <- sem(
  model = two_level_model, 
  data = df_1_modified, 
  cluster = "fam_id",
  missing = 'fiml',
  fixed.x = T
)

summary(fit_msem_fiml)

################
# Hypothesis 2 #
################

df_diff <- df_1
mpvs12 <- subtract_twins_values(df=df_diff, var="mpvs_total_12_1")
mpvs14 <- subtract_twins_values(df=df_diff, var="mpvs_total_14_1")
mpvs16 <- subtract_twins_values(df=df_diff, var="mpvs_total_16_1")
mpvs21_phase2 <- subtract_twins_values(df=df_diff, var="mpvs_total_21_phase_2_1")
dcq26 <- subtract_twins_values(df=df_diff, var="dcq_26_1")

df_diff <- left_join_multiple_df_diff_twin_values(
  left_df = mpvs12,
  right_dfs = list(mpvs14, mpvs16, mpvs21_phase2,dcq26)
)
df_diff[complete.cases(df_diff),] %>% View()

rm(list=c("mpvs12", "mpvs14", "mpvs16", "mpvs21_phase2", "dcq26"))

twin_diff_model <- '
    dcq_26_1 ~ mpvs_total_12_1
    dcq_26_1 ~ mpvs_total_14_1
    dcq_26_1 ~ mpvs_total_16_1
    dcq_26_1 ~ mpvs_total_21_phase_2_1
    mpvs_total_14_1 ~ mpvs_total_12_1
    mpvs_total_16_1 ~ mpvs_total_14_1
    mpvs_total_21_phase_2_1 ~ mpvs_total_16_1
'

fit1 <- sem(
  model = twin_diff_model, 
  data = df_diff,
  fixed.x = F,
  estimator="ML"
)

summary(fit1)

# Inspect missing pattern in our df
df_diff %>% 
  select(
    all_of(
      c(
        "dcq_26_1","mpvs_total_12_1","mpvs_total_14_1",
        "mpvs_total_16_1", "mpvs_total_21_phase_2_1"
      )
    )
  ) %>%
  mice::md.pattern(rotate.names =T)

fit2 <- sem(
  model = twin_diff_model, 
  data = df_diff,
  missing = 'fiml',
  fixed.x = F
)

summary(fit2)

