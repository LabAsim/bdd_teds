library(umx)
library(lavaan)

source("data_management_hyp_1.R")

################
# Hypothesis 1 #
################
#To test hypothesis 1,
# we will use Structural Equation Modeling (SEM) 
# in order to determine the longitudinal phenotypic association 
# of earlier peer victimization (MPVS score) with later BDD symptoms (DCQ score). 
# Potential confounders, namely age and sex 


# Without covid
model_scaled_32_without_covid <- '
    # DCQ
    dcq_total_26_1 ~ mpvs_total_12_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_child_14_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_16_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_phase_2_21_1_scaled_32
    
    # Mpvs
    mpvs_total_child_14_1_scaled_32 ~ mpvs_total_12_1_scaled_32
    mpvs_total_16_1_scaled_32 ~ mpvs_total_child_14_1_scaled_32
    mpvs_total_phase_2_21_1_scaled_32 ~ mpvs_total_16_1_scaled_32
    
    # Age
    dcq_total_26_1 ~ age_26_1 
    mpvs_total_12_1_scaled_32 ~ age_child_12_1
    mpvs_total_child_14_1_scaled_32 ~ age_child_14_1
    mpvs_total_16_1_scaled_32 ~ age_child_web_16_1
    mpvs_total_phase_2_21_1_scaled_32 ~ age_phase2_child_21_1
    
    # Sex
    dcq_total_26_1 ~ sex_1 
    mpvs_total_12_1_scaled_32 ~ sex_1
    mpvs_total_child_14_1_scaled_32 ~ sex_1
    mpvs_total_16_1_scaled_32 ~ sex_1
    mpvs_total_phase_2_21_1_scaled_32 ~ sex_1
'
fit_fiml_scaled_32_without_covid <- sem(
  model = model_scaled_32_without_covid, 
  data = df_essential_vars, 
  cluster = "fam_id",
  missing="fiml"
)
summary(fit_fiml_scaled_32_without_covid)

# With covid
model_scaled_32_with_covid <- '
    # DCQ
    dcq_total_26_1 ~ mpvs_total_12_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_child_14_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_16_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_phase_2_21_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_cov1_21_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_cov2_21_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_cov3_21_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_cov4_21_1_scaled_32
    
    # Mpvs
    mpvs_total_child_14_1_scaled_32 ~ mpvs_total_12_1_scaled_32
    mpvs_total_16_1_scaled_32 ~ mpvs_total_child_14_1_scaled_32
    mpvs_total_phase_2_21_1_scaled_32 ~ mpvs_total_16_1_scaled_32
    mpvs_total_cov1_21_1_scaled_32 ~ mpvs_total_phase_2_21_1_scaled_32
    mpvs_total_cov2_21_1_scaled_32 ~ mpvs_total_cov1_21_1_scaled_32
    mpvs_total_cov3_21_1_scaled_32 ~ mpvs_total_cov2_21_1_scaled_32
    mpvs_total_cov4_21_1_scaled_32 ~ mpvs_total_cov3_21_1_scaled_32
    
    # Age
    dcq_total_26_1 ~ age_26_1 
    mpvs_total_12_1_scaled_32 ~ age_child_12_1
    mpvs_total_child_14_1_scaled_32 ~ age_child_14_1
    mpvs_total_16_1_scaled_32 ~ age_child_web_16_1
    mpvs_total_phase_2_21_1_scaled_32 ~ age_phase2_child_21_1
    mpvs_total_cov2_21_1_scaled_32 ~ age_cov1_child_21_1
    mpvs_total_cov3_21_1_scaled_32 ~ age_cov2_child_21_1
    mpvs_total_cov3_21_1_scaled_32 ~ age_cov3_child_21_1
    mpvs_total_cov4_21_1_scaled_32 ~ age_cov4_child_21_1
    
    # Sex
    dcq_total_26_1 ~ sex_1 
    mpvs_total_12_1_scaled_32 ~ sex_1
    mpvs_total_child_14_1_scaled_32 ~ sex_1
    mpvs_total_16_1_scaled_32 ~ sex_1
    mpvs_total_phase_2_21_1_scaled_32 ~ sex_1
    mpvs_total_cov2_21_1_scaled_32 ~ sex_1
    mpvs_total_cov3_21_1_scaled_32 ~ sex_1
    mpvs_total_cov3_21_1_scaled_32 ~ sex_1
    mpvs_total_cov4_21_1_scaled_32 ~ sex_1
'
fit_fiml_scaled_32_with_covid <- sem(
  model = model_scaled_32_with_covid, 
  data = df_essential_vars, 
  cluster = "fam_id",
  missing="fiml"
)
summary(fit_fiml_scaled_32_with_covid)
beepr::beep("mario")
