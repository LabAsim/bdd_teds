library(tidyverse)
############
# Load dfs #
############
if (exists("df_essential_vars") == F){
  load(file = "G:\\data_wrangling.Rdata")
}

vars <- c(
  "school_cohort_fct",
  "fam_id",
  "twin_id",
  "sex_1",
  "zygosity_binary_fct" ,
  # Ses & ethnicity serve as auxiliary vars, 
  # because they have many obs and are loosely associated with
  # Bullying and BDD
  "ses_1st_contact",
  "ethnic_fct" ,
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
  colnames(df_1)[grepl(pattern="mpvs_total",x=colnames(df_1))],
  "dcq_total_26_1"
)

df_essential_vars <- df_1 %>% 
  dplyr::select(all_of(vars)) 

df_essential_vars <- scale_mpvs(
  df=df_essential_vars,
  scale_size = 32,
  from_vars = colnames(df_1)[grepl(pattern="mpvs_total",x=colnames(df_1))]
)
