library(tidyverse)
############
# Load dfs #
############
source("data_wrangling.R")
# if (exists("df_1") == F){
#   source("data_wrangling.R")
# }
# if (exists("df_essential_vars") == F){
#   load(file = "G:\\data_wrangling.Rdata")
#   source("helper.R")
# }

vars <- c(
  "school_cohort_fct",
  "fam_id",
  "twin_id",
  "sex_1",
  "sex_1_fct",
  "zygosity_binary_fct",
  "eating_diagnosis_fct_26_1",
  "eating_diagnosis_26_1",
  # Ses & ethnicity serve as auxiliary vars,
  # because they have many obs and are loosely associated with
  # Bullying and BDD
  "ses_1st_contact",
  "ethnic_fct",
  "age_parent_12",
  "age_teach_12_1",
  "age_child_12_1",
  "age_parent_14",
  "age_teach_14_1",
  "age_child_14_1",
  "age_child_web_16_1",
  "age_phase1_parent_21",
  "age_phase2_child_21_1",
  "age_cov1_child_21_1",
  "age_cov2_child_21_1",
  "age_cov3_child_21_1",
  "age_cov4_child_21_1",
  "age_26_1",
  colnames(df_1)[grepl(pattern = "mpvs_total", x = colnames(df_1))],
  "dcq_total_26_1",
  "dcq_total_26_1_cutoff17"
)

df_essential_vars <- df_1 %>%
  dplyr::select(all_of(vars))

df_essential_vars <- scale_mpvs(
  df = df_essential_vars,
  scale_size = 32,
  from_vars = colnames(df_essential_vars)[grepl(pattern = "mpvs_total", x = colnames(df_essential_vars))]
)

# Mean var across waves, without omitting NAs
mpvs_vars <- c(
  "mpvs_total_12_1_scaled_32",
  "mpvs_total_child_14_1_scaled_32",
  "mpvs_total_16_1_scaled_32",
  "mpvs_total_phase_2_21_1_scaled_32"
)

df_essential_vars <- cbind(
  df_essential_vars,
  data.frame(mpvs_mean_scaled_32 = rowMeans(df_essential_vars[, mpvs_vars], na.rm = T))
)
rm("mpvs_vars")
