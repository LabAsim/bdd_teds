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
  fix_different_twins_values(var="age_21_1") %>%
  fix_different_twins_values(var="age_16_1")

df_1_modified_complete_cases <- df_1_modified_complete_cases %>%
  drop_identical_values(
      var="mpvs_total_12_1", drop_same_value=T, drop_na = T
    ) %>%
  drop_identical_values(
    var="mpvs_total_21_scaled_32", drop_same_value=T, drop_na = T
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
  age_21_1
) %>% View()

within_fam_model <- '
  level: 1
    dcq_26_1 ~ mpvs_total_12_1
    dcq_26_1 ~ mpvs_total_14_child_1
    dcq_26_1 ~ mpvs_total_16_1
    dcq_26_1 ~ mpvs_total_21_scaled_32
    mpvs_total_14_child_1 ~ mpvs_total_12_1
    mpvs_total_16_1 ~ mpvs_total_14_child_1
    mpvs_total_21_scaled_32 ~ mpvs_total_16_1
  level: 2
    dcq_26_1 ~ mpvs_total_12_1
    dcq_26_1 ~ mpvs_total_14_child_1
    dcq_26_1 ~ mpvs_total_16_1
    dcq_26_1 ~ mpvs_total_21_scaled_32
    dcq_26_1 ~ age_26_1 
    mpvs_total_14_child_1 ~ mpvs_total_12_1
    mpvs_total_16_1 ~ mpvs_total_14_child_1
    mpvs_total_21_scaled_32 ~ mpvs_total_16_1
    mpvs_total_12_1 ~ age_12_1
    mpvs_total_14_child_1 ~ age_14_1
    mpvs_total_16_1 ~ age_16_1
    mpvs_total_21_scaled_32 ~ age_21_1
'
fit1 <- sem(
  model = within_fam_model, 
  data = df_1_modified_complete_cases, 
  cluster = "fam_id"
)

summary(fit1)


# Retain NA values # 

df_1_modified <-  df_1 %>%
  fix_different_twins_values(var="age_parent_12") %>%
  fix_different_twins_values(var="age_teach_12_1") %>%
  fix_different_twins_values(var="age_child_12_1") %>%
  fix_different_twins_values(var="age_parent_14") %>%
  fix_different_twins_values(var="age_teach_14_1") %>%
  fix_different_twins_values(var="age_parent_16") %>%
  fix_different_twins_values(var="age_web_16_1") %>%
  fix_different_twins_values(var="age_child_16_1") %>%
  fix_different_twins_values(var="age_leap_study_parent_16") %>%
  fix_different_twins_values(var="age_phase2_child_21_1") %>%
  fix_different_twins_values(var="age_cov1_child_21_1") %>%
  fix_different_twins_values(var="age_cov2_child_21_1") %>%
  fix_different_twins_values(var="age_cov3_child_21_1") %>%
  fix_different_twins_values(var="age_cov4_child_21_1") %>%
  fix_different_twins_values(var="age_26_1") 
beepr::beep("mario")

df_1_modified <- df_1_modified %>%
  drop_identical_values(
    var="mpvs_total_12_1", drop_same_value=T, drop_na = F
  ) %>%
  drop_identical_values(
    var="mpvs_total_scaled_32", drop_same_value=T, drop_na = F
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
beepr::beep("mario")

df_1_modified %>% select(
  twin_id, fam_id, dcq_26_1, mpvs_total_12_1, mpvs_total_14_child_1,
  mpvs_total_16_1, mpvs_total_21_scaled_32, age_12_1,age_14_1, age_16_1,
  age_21_1
) %>% View()

df_1_modified %>% 
  select(
    dcq_26_1, mpvs_total_12_1, mpvs_total_14_child_1,
    mpvs_total_16_1, mpvs_total_21_scaled_32, age_12_1,age_14_1, age_16_1,
    age_21_1
  ) %>% 
  mice::md.pattern(rotate.names = T, plot = F) %>% 
  View()

two_level_model <- '
  level: 1
    dcq_26_1 ~ mpvs_total_12_1
    dcq_26_1 ~ mpvs_total_14_child_1
    dcq_26_1 ~ mpvs_total_16_1
    dcq_26_1 ~ mpvs_total_21_scaled_32
    mpvs_total_14_child_1 ~ mpvs_total_12_1
    mpvs_total_16_1 ~ mpvs_total_14_child_1
    mpvs_total_21_scaled_32 ~ mpvs_total_16_1
  level: 2
    dcq_26_1 ~ mpvs_total_12_1
    dcq_26_1 ~ mpvs_total_14_child_1
    dcq_26_1 ~ mpvs_total_16_1
    dcq_26_1 ~ mpvs_total_21_scaled_32
    dcq_26_1 ~ age_26_1 
    mpvs_total_14_child_1 ~ mpvs_total_12_1
    mpvs_total_16_1 ~ mpvs_total_14_child_1
    mpvs_total_21_scaled_32 ~ mpvs_total_16_1
    mpvs_total_12_1 ~ age_12_1
    mpvs_total_14_child_1 ~ age_14_1
    mpvs_total_16_1 ~ age_16_1
    mpvs_total_21_scaled_32 ~ age_21_1
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


fit_msem_fiml2 <- sem(
  model = two_level_model, 
  data = df_1_modified, 
  cluster = "fam_id",
  missing = 'fiml',
  fixed.x = F
)

summary(fit_msem_fiml2)

################
# Hypothesis 2 #
################

df_diff <- df_1
mpvs12 <- subtract_twins_values(df=df_diff, var="mpvs_total_12_1")
mpvs14 <- subtract_twins_values(df=df_diff, var="mpvs_total_14_1")
mpvs16 <- subtract_twins_values(df=df_diff, var="mpvs_total_16_1")
mpvs21_phase2 <- subtract_twins_values(df=df_diff, var="mpvs_total_21_phase_2_1")
mpvs_total_21_scaled_32 <- subtract_twins_values(df=df_diff, var="mpvs_total_21_scaled_32")
dcq26 <- subtract_twins_values(df=df_diff, var="dcq_26_1")

df_diff <- left_join_multiple_df_diff_twin_values(
  left_df = mpvs12,
  right_dfs = list(mpvs14, mpvs16, mpvs_total_21_scaled_32,dcq26)
)
df_diff[complete.cases(df_diff),] %>% View()

rm(
  list=c(
    "mpvs12", "mpvs14", "mpvs16", "mpvs21_phase2",
    "mpvs_total_21_scaled_32", "dcq26"
  )
)

twin_diff_model <- '
    dcq_26_1 ~ mpvs_total_12_1
    dcq_26_1 ~ mpvs_total_14_1
    dcq_26_1 ~ mpvs_total_16_1
    dcq_26_1 ~ mpvs_total_21_scaled_32
    mpvs_total_14_1 ~ mpvs_total_12_1
    mpvs_total_16_1 ~ mpvs_total_14_1
    mpvs_total_21_scaled_32 ~ mpvs_total_16_1
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
        "mpvs_total_16_1", "mpvs_total_21_scaled_32"
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

df_diff_scaled <- scale_mpvs(df=df_1, scale_size = 32)
mpvs12 <- subtract_twins_values(df=df_diff_scaled, var="mpvs_total_12_1_scaled_32")
mpvs14 <- subtract_twins_values(df=df_diff_scaled, var="mpvs_total_14_1_scaled_32")
mpvs16 <- subtract_twins_values(df=df_diff_scaled, var="mpvs_total_16_1_scaled_32")
mpvs21_phase2 <- subtract_twins_values(df=df_diff_scaled, var="mpvs_total_21_phase_2_1_scaled_32")
mpvs_total_21 <- subtract_twins_values(df=df_diff_scaled, var="mpvs_total_21_scaled_32")
dcq26 <- subtract_twins_values(df=df_diff_scaled, var="dcq_26_1")

df_diff_scaled <- left_join_multiple_df_diff_twin_values(
  left_df = mpvs12,
  right_dfs = list(mpvs14, mpvs16, mpvs21_phase2,mpvs_total_21,dcq26)
)
df_diff_scaled[complete.cases(df_diff_scaled),] %>% View()

rm(list=c("mpvs12", "mpvs14", "mpvs16", "mpvs21_phase2","mpvs_total_21", "dcq26"))

twin_diff_model_scaled <- '
    dcq_26_1 ~ mpvs_total_12_1_scaled_32
    dcq_26_1 ~ mpvs_total_14_1_scaled_32
    dcq_26_1 ~ mpvs_total_16_1_scaled_32
    dcq_26_1 ~ mpvs_total_21_scaled_32
    mpvs_total_14_1_scaled_32 ~ mpvs_total_12_1_scaled_32
    mpvs_total_16_1_scaled_32 ~ mpvs_total_14_1_scaled_32
    mpvs_total_21_scaled_32 ~ mpvs_total_16_1_scaled_32
'

fit1_scaled <- sem(
  model = twin_diff_model_scaled, 
  data = df_diff_scaled,
  fixed.x = F,
  estimator="ML"
)
summary(fit1_scaled)


fit2_scaled <- sem(
  model = twin_diff_model_scaled, 
  data = df_diff_scaled,
  missing = 'fiml',
  fixed.x = F
)

summary(fit2_scaled)


######################
# Using imputed data #
######################

source("imputation_derived.R")

################
# Hypothesis 1 #
################

df_1_modified_complete_cases <-  drop_identical_fix_different_valeus(
  df=data,
  fix_vec = colnames(data)[grepl(pattern="age", x=colnames(data))],
  drop_identical_vec = colnames(data)[grepl(pattern="scaled", x=colnames(data))]
) %>% drop_identical_fix_different_valeus(
  fix_vec = c(),
  drop_identical_vec = c("dcq_26_1")
)

within_fam_model <- '
  level: 1
    dcq_26_1 ~ mpvs_total_12_1_scaled_32
    dcq_26_1 ~ mpvs_total_14_1_scaled_32
    dcq_26_1 ~ mpvs_total_16_1_scaled_32
    dcq_26_1 ~ mpvs_total_21_scaled_32
    dcq_26_1 ~ sex_1
    mpvs_total_14_1_scaled_32 ~ mpvs_total_12_1_scaled_32
    mpvs_total_16_1_scaled_32 ~ mpvs_total_14_1_scaled_32
    mpvs_total_21_scaled_32 ~ mpvs_total_16_1_scaled_32
    mpvs_total_12_1_scaled_32 ~ sex_1
    mpvs_total_14_1_scaled_32 ~ sex_1
    mpvs_total_16_1_scaled_32 ~ sex_1
    mpvs_total_21_scaled_32 ~ sex_1
  level: 2
    dcq_26_1 ~ mpvs_total_12_1_scaled_32
    dcq_26_1 ~ mpvs_total_14_1_scaled_32
    dcq_26_1 ~ mpvs_total_16_1_scaled_32
    dcq_26_1 ~ mpvs_total_21_scaled_32
    dcq_26_1 ~ age_26_1 
    dcq_26_1 ~ sex_1
    mpvs_total_14_1_scaled_32 ~ mpvs_total_12_1_scaled_32
    mpvs_total_16_1_scaled_32 ~ mpvs_total_14_1_scaled_32
    mpvs_total_21_scaled_32 ~ mpvs_total_16_1_scaled_32
    mpvs_total_12_1_scaled_32 ~ age_12_1
    mpvs_total_14_1_scaled_32 ~ age_14_1
    mpvs_total_16_1_scaled_32 ~ age_16_1
    mpvs_total_21_scaled_32 ~ age_21_1
    mpvs_total_12_1_scaled_32 ~ sex_1
    mpvs_total_14_1_scaled_32 ~ sex_1
    mpvs_total_16_1_scaled_32 ~ sex_1
    mpvs_total_21_scaled_32 ~ sex_1
'
fit1 <- sem(
  model = within_fam_model, 
  data = df_1_modified_complete_cases, 
  cluster = "fam_id"
)

summary(fit1)

###############
#Hypothesis 2 #
###############



df_diff_scaled <- data %>%
  create_df_subtract_twins_values_multiple_vars(
    group_var="fam_id",
    vars = c(
      "mpvs_total_12_1_scaled_32",
      "mpvs_total_14_1_scaled_32",
      "mpvs_total_16_1_scaled_32",
      "mpvs_total_21_scaled_32",
      "dcq_26_1"
    )
  )
# mpvs12 <- subtract_twins_values(df=df_diff_scaled, var="mpvs_total_12_1_scaled_32")
# mpvs14 <- subtract_twins_values(df=df_diff_scaled, var="mpvs_total_14_1_scaled_32")
# mpvs16 <- subtract_twins_values(df=df_diff_scaled, var="mpvs_total_16_1_scaled_32")
# mpvs21_phase2 <- subtract_twins_values(df=df_diff_scaled, var="mpvs_total_21_phase_2_1_scaled_32")
# mpvs_total_21 <- subtract_twins_values(df=df_diff_scaled, var="mpvs_total_21_scaled_32")
# dcq26 <- subtract_twins_values(df=df_diff_scaled, var="dcq_26_1")
# 
# df_diff_scaled <- left_join_multiple_df_diff_twin_values(
#   left_df = mpvs12,
#   right_dfs = list(mpvs14, mpvs16, mpvs21_phase2,mpvs_total_21,dcq26)
# )

twin_diff_model_scaled <- '
    dcq_26_1 ~ mpvs_total_12_1_scaled_32
    dcq_26_1 ~ mpvs_total_14_1_scaled_32
    dcq_26_1 ~ mpvs_total_16_1_scaled_32
    dcq_26_1 ~ mpvs_total_21_scaled_32
    mpvs_total_14_1_scaled_32 ~ mpvs_total_12_1_scaled_32
    mpvs_total_16_1_scaled_32 ~ mpvs_total_14_1_scaled_32
    mpvs_total_21_scaled_32 ~ mpvs_total_16_1_scaled_32
'

fit3_scaled <- sem(
  model = twin_diff_model_scaled, 
  data = data,
  missing = "ml"
)

summary(fit3_scaled)

